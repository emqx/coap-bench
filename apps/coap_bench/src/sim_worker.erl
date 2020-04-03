-module(sim_worker).

-behaviour(gen_statem).

-export([start_link/3]).

-export([init/1, callback_mode/0, terminate/3]).

-export([response_received/2]).

-export([working/3, sleep/3, wait_coap_msg/3]).

-define(ACK_TIMEOUT, 15000).
-define(MAX_MESSAGE_ID, 65535). % 16-bit number

-define(handle_events(EventType, EventContent, Data),
        handle_common_events(?FUNCTION_NAME, EventType, EventContent, Data)).

-record(data, {
            sock,
            conf = #{},
            nextmid,
            location = [],
            observed = #{},
            workflow :: [],
            verify_msg :: fun((term()) -> {ok, #data{}} | {error, term()})
        }).

start_link(WorkFlow, Vars, Conf) ->
    gen_statem:start_link(?MODULE, [WorkFlow, Vars, Conf], []).

response_received(Pid, Task) ->
    gen_statem:cast(Pid, Task).

init([WorkFlow, Vars, Conf = #{binds := Binds}]) ->
    {ok, Sock} = gen_udp:open(0, [{ip, pick(Binds)}, binary, {active, true}, {reuseaddr, true}]),
    {ok, working, #data{
            sock = Sock,
            conf = Conf,
            nextmid = first_mid(),
            workflow = trans_workflow(WorkFlow, Vars),
            verify_msg = undefined
        }, [{next_event, internal, continue_workflow}]}.

callback_mode() ->
    [state_functions].

working(internal, continue_workflow, Data) ->
    logger:debug("[~p] continue workflow: ~p", [?MODULE, Data#data.workflow]),
    process_task(Data);
working(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

wait_coap_msg(enter, _OldState, _Data) ->
    keep_state_and_data;
wait_coap_msg(info, {udp, Sock, _PeerIP, _PeerPortNo, Packet},
             #data{verify_msg = Validitor, sock = Sock} = Data) when is_function(Validitor) ->
    try
        lwm2m_coap_message_parser:decode(Packet)
    of
        CoapMsg ->
            case Validitor(CoapMsg, Data) of
                {ok, StateData} ->
                    continue_working(StateData#data{verify_msg = undefined});
                {error, Reason} ->
                    logger:error("received coap message that not expected: ~p, reason: ~p", [CoapMsg, Reason]),
                    keep_state_and_data
            end
    catch
        _:_ ->
            logger:error("received udp message that not expected: ~p", [Packet]),
            keep_state_and_data
    end;
wait_coap_msg(state_timeout, wait_msg_timeout, _Data) ->
    {stop, {shutdown, wait_msg_timeout}};
wait_coap_msg(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

sleep(enter, _OldState, _Data) ->
    keep_state_and_data;
sleep(state_timeout, wakeup, Data) ->
    continue_working(Data);
sleep(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

%% -------------------------------------------
%% Process tasks and tranform the state
%% -------------------------------------------

process_task(#data{workflow = []}) ->
    {stop, {shutdown, workflow_complete}};

process_task(#data{workflow = [{register, #{ep := Ep, lifetime := Lifetime, object_links := ObjectLinks}} | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor =
        fun(RcvdMsg, StateData0) ->
            case {lwm2m_cmd:ack_validator(RcvdMsg, MsgId), lwm2m_cmd:location_path(RcvdMsg)} of
                {_, []} -> {error, no_location_path};
                {true, LocationPath} ->
                    {ok, StateData0#data{location = LocationPath}}
            end
        end,
    send_request(Sock, Host, Port, lwm2m_cmd:make_register(Ep, Lifetime, MsgId, ObjectLinks), Data, WorkFlow, MsgId, Validitor);

process_task(#data{workflow = [{deregister, #{}} | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   location = Location,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor =
        fun(RcvdMsg, StateData0) ->
            case lwm2m_cmd:ack_validator(RcvdMsg, MsgId) of
                true -> {ok, StateData0};
                false -> {error, ack_not_matched}
            end
        end,
    send_request(Sock, Host, Port, lwm2m_cmd:make_deregister(Location, MsgId), Data, WorkFlow, MsgId, Validitor);

process_task(#data{workflow = [{wait_observe, #{path := Path, timeout := Sec} = BodyOpts} | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor =
        fun(RcvdMsg, StateData0 = #data{observed = Observed}) ->
            case {lwm2m_cmd:uri_path(RcvdMsg), lwm2m_cmd:token(RcvdMsg)} of
                {Path0, Token0} when Path0 =:= Path ->
                    Ack = lwm2m_cmd:make_ack(RcvdMsg, {ok, content}, make_body(BodyOpts),
                            [{content_format, <<"application/vnd.oma.lwm2m+tlv">>}]),
                    gen_udp:send(Sock, Host, Port, lwm2m_coap_message_parser:encode(Ack)),
                    {ok, StateData0#data{observed = Observed#{Path0 => Token0}}};
                {_Path0, _Token0} ->
                    {error, observe_path_not_matched}
            end
        end,
    {next_state, wait_coap_msg, Data#data{workflow = WorkFlow, verify_msg = Validitor, nextmid = next_mid(MsgId)},
        [{state_timeout, timer:seconds(Sec), wait_msg_timeout}]};

process_task(#data{workflow = [{notify, #{path := Path} = BodyOpts} | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   observed = Observed,
                   conf = #{host := Host, port := Port}} = Data) ->
    case maps:find(Path, Observed) of
        {ok, Token} ->
            Notify = lwm2m_cmd:make_notify(Token, MsgId, make_body(BodyOpts)),
            send_response(Sock, Host, Port, Notify, Data, WorkFlow, MsgId);
        error ->
            {stop, {shutdown, {not_observed, Path}}}
    end;

process_task(#data{workflow = [{sleep, #{interval := Sec}} | WorkFlow]} = Data) ->
    {next_state, sleep, Data#data{workflow = WorkFlow},
        [{state_timeout, timer:seconds(Sec), wakeup}]};

process_task(#data{workflow = [Flow | WorkFlow]} = Data) ->
    logger:error("unknow workflow: ~p", [Flow]),
    {keep_state, Data#data{workflow = WorkFlow}, []}.

handle_common_events(StateName, EventType, Event, _Data) ->
    logger:warning("received unexpected event: ~p in state: ~p", [{EventType, Event}, StateName]),
    keep_state_and_data.

terminate(Reason, _State, #data{workflow = [], sock = Sock}) ->
    gen_udp:close(Sock),
    logger:debug("[~p] terminate: ~p", [?MODULE, Reason]);
terminate(Reason, _State, #data{workflow = Tasks, sock = Sock}) ->
    gen_udp:close(Sock),
    logger:error("[~p] terminate with pending tasks: ~p, reason: ~p", [?MODULE, Tasks, Reason]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_request(Sock, Host, Port, CoapMsg, StateData, RemWorkFlow, MsgId, Validitor) ->
    case gen_udp:send(Sock, Host, Port, lwm2m_coap_message_parser:encode(CoapMsg)) of
        ok ->
            {next_state, wait_coap_msg,
                StateData#data{
                    workflow = RemWorkFlow,
                    nextmid = next_mid(MsgId),
                    verify_msg = Validitor
                },
                [{state_timeout, ?ACK_TIMEOUT, wait_msg_timeout}]};
        {error, Reason} ->
            logger:error("send request failed: ~p", [Reason]),
            {stop, {shutdown, Reason}}
    end.
send_response(Sock, Host, Port, CoapMsg, StateData, RemWorkFlow, MsgId) ->
    case gen_udp:send(Sock, Host, Port, lwm2m_coap_message_parser:encode(CoapMsg)) of
        ok ->
             continue_working(StateData#data{
                 nextmid = next_mid(MsgId),
                 verify_msg = undefined,
                 workflow = RemWorkFlow});
        {error, Reason} ->
            logger:error("send request failed: ~p", [Reason]),
            {stop, {shutdown, Reason}}
    end.

continue_working(StateData) ->
    {next_state, working, StateData, [{next_event, internal, continue_workflow}]}.

trans_workflow(WorkFlow, Data0) when is_list(WorkFlow) ->
    Data = [bin(Tk) || Tk <- string:split(string:tokens(Data0, "\n"), ",", all)],
    [do_trans_workflow(Flow, Data) || Flow <- WorkFlow].

do_trans_workflow({FlowName, Opts}, Data) when is_map(Opts) ->
    {FlowName, coap_bench_utils:replace_map_var(Opts, Data)}.

make_body(#{body := auto_gen_binary, size := Size} = _Opts) ->
    crypto:strong_rand_bytes(Size);
make_body(#{body := Body}) when is_binary(Body) ->
    Body.

first_mid() ->
    rand:uniform(?MAX_MESSAGE_ID).

next_mid(MsgId) ->
    if  MsgId < ?MAX_MESSAGE_ID -> MsgId + 1;
        true -> 1 % or 0?
    end.

pick(List) ->
    lists:nth(rand:uniform(length(List)), List).

bin(Tk) when is_binary(Tk) -> Tk;
bin(Tk) when is_list(Tk) -> list_to_binary(Tk).
