-module(sim_worker).

-behaviour(gen_statem).

-export([start_link/4]).

-export([init/1, callback_mode/0, terminate/3]).

-export([send_msg/4]).

-export([working/3, sleep/3, wait_coap_msg/3]).

-define(MAX_MESSAGE_ID, 65535). % 16-bit number

-define(handle_events(EventType, EventContent, Data),
        handle_common_events(?FUNCTION_NAME, EventType, EventContent, Data)).

-record(data, {
            sock,
            sockname,
            conf = #{},
            nextmid,
            location = [],
            observed = #{},
            workflow :: [],
            current_task,
            verify_msg :: fun((term()) -> {ok, #data{}} | {error, term()})
        }).

start_link(WorkFlow, Vars, Sock, Conf) ->
    gen_statem:start_link(?MODULE, [WorkFlow, Vars, Sock, Conf], []).

send_msg(Sock, Host, Port, CoapMsg) ->
    case gen_udp:send(Sock, Host, Port, lwm2m_coap_message_parser:encode(CoapMsg)) of
        {error, Reason} ->
            coap_bench_message:incr_counter_send_fail(CoapMsg),
            {error, Reason};
        ok ->
            coap_bench_message:incr_counter_sent(CoapMsg),
            ok
    end.

init([WorkFlow, Vars, Sock, Conf]) ->
    {ok, SockName} = inet:sockname(Sock),
    inet:setopts(Sock, [{active, true}]),
    {ok, working, #data{
            sock = Sock,
            sockname = SockName,
            conf = Conf,
            nextmid = first_mid(),
            workflow = trans_workflow(WorkFlow, Vars),
            current_task = undefined,
            verify_msg = undefined
        }, [{next_event, internal, continue_workflow}]}.

callback_mode() ->
    [state_functions].

working(internal, continue_workflow, Data) ->
    logger:debug("[~p] continue workflow: ~p", [?MODULE, Data#data.workflow]),
    process_task(Data);
working(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

wait_coap_msg(info, {udp, Sock, _PeerIP, _PeerPortNo, Packet},
             #data{verify_msg = Validitor, sock = Sock} = Data) when is_function(Validitor) ->
    try
        lwm2m_coap_message_parser:decode(Packet)
    of
        CoapMsg ->
            coap_bench_message:incr_counter_rcvd(CoapMsg),
            case Validitor(CoapMsg, Data) of
                {ok, StateData} ->
                    continue_working(StateData#data{verify_msg = undefined});
                {error, Reason} ->
                    logger:error("received coap message that not expected: ~p, reason: ~p", [CoapMsg, Reason]),
                    keep_state_and_data
            end
    catch
        _:_ ->
            logger:error("received unknown udp message that not expected: ~p", [Packet]),
            keep_state_and_data
    end;
wait_coap_msg(state_timeout, wait_msg_timeout, #data{verify_msg = Validitor} = Data) ->
    Validitor(timeout, Data),
    {stop, {shutdown, wait_msg_timeout}};
wait_coap_msg(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

sleep(state_timeout, wakeup, Data) ->
    continue_working(Data);
sleep(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

%% -------------------------------------------
%% Process tasks and tranform the state
%% -------------------------------------------

process_task(#data{workflow = []} = Data) ->
    {stop, {shutdown, workflow_complete}, Data#data{current_task = undefined}};

process_task(#data{workflow = [{register, #{ep := Ep, lifetime := Lifetime, object_links := ObjectLinks, timeout := Timeout}} = Task | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor =
        fun(RcvdMsg, StateData0) ->
            case {coap_bench_message:ack_validator(RcvdMsg, MsgId), coap_bench_message:location_path(RcvdMsg)} of
                {_, []} ->
                    coap_bench_metrics:incr('REGISTER_FAIL'),
                    {error, {no_location_path, RcvdMsg}};
                {false, _} ->
                    coap_bench_metrics:incr('REGISTER_FAIL'),
                    {error, {ack_not_matched, MsgId, RcvdMsg}};
                {true, LocationPath} ->
                    coap_bench_metrics:incr('REGISTER_SUCC'),
                    {ok, StateData0#data{location = LocationPath}}
            end;
           (timeout, _StateData0) ->
               coap_bench_metrics:incr('REGISTER_TIMEOUT')
        end,
    coap_bench_metrics:incr('REGISTER'),
    send_request(Sock, Host, Port, coap_bench_message:make_register(Ep, Lifetime, MsgId, ObjectLinks), Timeout, Data#data{current_task = Task}, WorkFlow, MsgId, Validitor);

process_task(#data{workflow = [{deregister, #{timeout := Timeout}} = Task | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   location = Location,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor =
        fun(RcvdMsg, StateData0) ->
            case coap_bench_message:ack_validator(RcvdMsg, MsgId) of
                true ->
                    coap_bench_metrics:incr('DEREGISTER_SUCC'),
                    {ok, StateData0};
                false ->
                    coap_bench_metrics:incr('DEREGISTER_FAIL'),
                    {error, {ack_not_matched, MsgId, RcvdMsg}}
            end;
           (timeout, _StateData0) ->
               coap_bench_metrics:incr('DEREGISTER_TIMEOUT')
        end,
    coap_bench_metrics:incr('DEREGISTER'),
    send_request(Sock, Host, Port, coap_bench_message:make_deregister(Location, MsgId), Timeout, Data#data{current_task = Task}, WorkFlow, MsgId, Validitor);

process_task(#data{workflow = [{wait_observe, #{path := Path, timeout := Sec, content_format := ContentFormat} = BodyOpts} = Task | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor =
        fun(RcvdMsg, StateData0 = #data{observed = Observed}) ->
            case {coap_bench_message:uri_path(RcvdMsg), coap_bench_message:token(RcvdMsg)} of
                {Path0, Token0} when Path0 =:= Path, is_binary(Token0) ->
                    Ack = coap_bench_message:make_ack(RcvdMsg, {ok, content}, make_body(BodyOpts),
                            [{observe, 0}, {content_format, ContentFormat}]),
                    send_msg(Sock, Host, Port, Ack),
                    coap_bench_metrics:incr('WAIT_OBSERVE_SUCC'),
                    {ok, StateData0#data{observed = Observed#{Path0 => Token0}}};
                {Path0, Token0} ->
                    coap_bench_metrics:incr('WAIT_OBSERVE_FAIL'),
                    {error, {observe_path_not_matched, Path0, Token0}}
            end;
           (timeout, _StateData0) ->
               coap_bench_metrics:incr('WAIT_OBSERVE_TIMEOUT')
        end,
    coap_bench_metrics:incr('WAIT_OBSERVE'),
    {next_state, wait_coap_msg, Data#data{current_task = Task, workflow = WorkFlow, verify_msg = Validitor, nextmid = next_mid(MsgId)},
        [{state_timeout, timer:seconds(Sec), wait_msg_timeout}]};

process_task(#data{workflow = [{notify, #{path := Path, content_format := ContentFormat} = BodyOpts} = Task | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   observed = Observed,
                   conf = #{host := Host, port := Port}} = Data) ->
    case maps:find(Path, Observed) of
        {ok, Token} ->
            coap_bench_metrics:incr('NOTIFY'),
            Notify = coap_bench_message:make_notify(non, Token, MsgId, make_body(BodyOpts), MsgId, ContentFormat),
            send_response(Sock, Host, Port, Notify, Data#data{current_task = Task}, WorkFlow, MsgId);
        error ->
            {stop, {shutdown, {not_observed, Path}}, Data#data{current_task = Task}}
    end;

process_task(#data{workflow = [{sleep, #{interval := Sec}} = Task | WorkFlow]} = Data) ->
    {next_state, sleep, Data#data{workflow = WorkFlow, current_task = Task},
        [{state_timeout, timer:seconds(Sec), wakeup}, hibernate]};

process_task(#data{workflow = [Task | WorkFlow]} = Data) ->
    logger:error("unknow workflow: ~p", [Task]),
    {keep_state, Data#data{current_task = Task, workflow = WorkFlow}, []}.

handle_common_events(StateName, EventType, Event, _Data) ->
    logger:warning("received unexpected event: ~p in state: ~p", [{EventType, Event}, StateName]),
    common_events_state(StateName).

common_events_state(sleep) ->
    {keep_state_and_data, [hibernate]};
common_events_state(_StateName) ->
    keep_state_and_data.

terminate(Reason, _State, #data{workflow = [], sock = Sock, sockname = SockName}) ->
    logger:debug("[~p] terminate: ~p, sockname: ~p", [?MODULE, Reason, SockName]),
    sim_trash:take_socket(Sock);
terminate(Reason, _State, #data{workflow = Tasks, current_task = Task, sock = Sock, sockname = SockName}) ->
    logger:error("[~p] terminate when running task: ~p, pending tasks in the queue: ~p, reason: ~p, sockname: ~p", [?MODULE, Task, Tasks, Reason, SockName]),
    sim_trash:take_socket(Sock).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_request(Sock, Host, Port, CoapMsg, Timeout, StateData, RemWorkFlow, MsgId, Validitor) ->
    case send_msg(Sock, Host, Port, CoapMsg) of
        ok ->
            {next_state, wait_coap_msg,
                StateData#data{
                    workflow = RemWorkFlow,
                    nextmid = next_mid(MsgId),
                    verify_msg = Validitor
                },
                [{state_timeout, timer:seconds(Timeout), wait_msg_timeout}]};
        {error, Reason} ->
            logger:error("send request failed: ~p", [Reason]),
            {stop, {shutdown, Reason}}
    end.
send_response(Sock, Host, Port, CoapMsg, StateData, RemWorkFlow, MsgId) ->
    case send_msg(Sock, Host, Port, CoapMsg) of
        ok -> continue_working(StateData#data{
                 nextmid = next_mid(MsgId),
                 verify_msg = undefined,
                 workflow = RemWorkFlow});
        {error, Reason} ->
            logger:error("send request failed: ~p", [Reason]),
            {stop, {shutdown, Reason}}
    end.

continue_working(StateData) ->
    {next_state, working, StateData, [{next_event, internal, continue_workflow}]}.

trans_workflow(WorkFlow, Vars0) when is_list(WorkFlow) ->
    Vars = [bin(Tk) || Tk <- string:split(string:tokens(Vars0, "\n"), ",", all)],
    [do_trans_workflow(Flow, Vars) || Flow <- WorkFlow].

do_trans_workflow({FlowName, Opts}, Vars) when is_map(Opts) ->
    {FlowName, coap_bench_utils:replace_map_var(Opts, Vars)}.

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

bin(Tk) when is_binary(Tk) -> Tk;
bin(Tk) when is_list(Tk) -> list_to_binary(Tk).
