-module(coap_sim_worker).

-behaviour(gen_statem).

-export([start_link/5, resume/1]).

-export([init/1, callback_mode/0, terminate/3]).

-export([send_msg/4, may_ack_it/4]).

-export([ working/3
        , sleep/3
        , pause/3
        , wait_coap_msg/3
        ]).

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
            allow_retry_cons = #{},
            workflow :: [],
            on_unexpected_msg :: #{},
            current_task,
            verify_msg :: fun((term()) -> {ok, #data{}} | {error, term()})
        }).

start_link(WorkFlow, OnCoapMsgRcvd, Vars, Sock, Conf) ->
    gen_statem:start_link(?MODULE, [WorkFlow, OnCoapMsgRcvd, Vars, Sock, Conf], []).

resume(SimWorker) ->
    gen_statem:cast(SimWorker, resume).

send_msg(Sock, Host, Port, CoapMsg) ->
    case gen_udp:send(Sock, Host, Port, lwm2m_coap_message_parser:encode(CoapMsg)) of
        {error, Reason} ->
            coap_bench_message:incr_counter_send_fail(CoapMsg),
            {error, Reason};
        ok ->
            coap_bench_message:incr_counter_sent(CoapMsg),
            ok
    end.

may_ack_it(Sock, PeerIP, PeerPortNo, CoapMsg) ->
    case coap_bench_message:type(CoapMsg) of
        con -> coap_sim_worker:send_msg(Sock, PeerIP, PeerPortNo,
                    coap_bench_message:make_empty_ack(CoapMsg));
        _ -> ok
    end.

init([WorkFlow, OnUnexpectedMsg, Vars, Sock, Conf0]) ->
    {Hosts, Conf} = maps:take(hosts, Conf0),
    Host = lists:nth(rand:uniform(length(Hosts)), Hosts),
    {ok, SockName} = inet:sockname(Sock),
    inet:setopts(Sock, [{active, true}]),
    {ok, working, #data{
            sock = Sock,
            sockname = SockName,
            conf = Conf#{host => Host},
            nextmid = first_mid(),
            workflow = trans_workflow(WorkFlow, Vars),
            on_unexpected_msg = OnUnexpectedMsg,
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

wait_coap_msg(info, {udp, Sock, PeerIP, PeerPortNo, Packet},
             #data{verify_msg = Validitor, sock = Sock,
                   on_unexpected_msg = OnUnexpectedMsg} = Data) when is_function(Validitor) ->
    try
        lwm2m_coap_message_parser:decode(Packet)
    of
        CoapMsg ->
            coap_bench_message:incr_counter_rcvd(CoapMsg),
            case Validitor(CoapMsg, Data) of
                {ok, StateData} ->
                    continue_working(StateData#data{verify_msg = undefined});
                {error, ignore} ->
                    keep_state_and_data;
                {error, Reason} ->
                    logger:error("received coap message that not expected: ~p, reason: ~p, statedata: ~p", [CoapMsg, Reason, printable_data(Data)]),
                    handle_unexpected_coap_msg(Sock, PeerIP, PeerPortNo, CoapMsg, OnUnexpectedMsg)
            end
    catch
        _:_ ->
            logger:error("received unknown udp message that not expected: ~p, statedata: ~p", [Packet, printable_data(Data)]),
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

pause(cast, resume, Data) ->
    coap_bench_metrics:incr('PAUSE', -1),
    continue_working(Data);
pause(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

%% -------------------------------------------
%% Process tasks and tranform the state
%% -------------------------------------------

process_task(#data{workflow = []} = Data) ->
    {stop, {shutdown, workflow_complete}, Data#data{current_task = undefined}};

process_task(#data{workflow = [{register, #{ep := Ep, lifetime := Lifetime, object_links := ObjectLinks, timeout := MillSec}} = Task | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor = fun
        (timeout, _StateData0) ->
            coap_bench_metrics:incr('REGISTER_TIMEOUT');
        (RcvdMsg, StateData0) ->
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
            end
        end,
    coap_bench_metrics:incr('REGISTER'),
    Register = coap_bench_message:make_register(Ep, Lifetime, MsgId, ObjectLinks),
    send_request(Sock, Host, Port,
        Register, MillSec,
        Data#data{current_task = Task, workflow = WorkFlow},
        MsgId, Validitor);

process_task(#data{workflow = [{deregister, #{timeout := MillSec}} = Task | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   location = Location,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor = fun
        (timeout, _StateData0) ->
            coap_bench_metrics:incr('DEREGISTER_TIMEOUT');
        (RcvdMsg, StateData0) ->
            case coap_bench_message:ack_validator(RcvdMsg, MsgId) of
                true ->
                    coap_bench_metrics:incr('DEREGISTER_SUCC'),
                    {ok, StateData0};
                false ->
                    coap_bench_metrics:incr('DEREGISTER_FAIL'),
                    {error, {ack_not_matched, MsgId, RcvdMsg}}
            end
        end,
    coap_bench_metrics:incr('DEREGISTER'),
    Deregister = coap_bench_message:make_deregister(Location, MsgId),
    send_request(Sock, Host, Port, Deregister, MillSec,
        Data#data{current_task = Task, workflow = WorkFlow},
        MsgId, Validitor);

process_task(#data{workflow = [{wait_observe, #{path := Path, timeout := MillSec, content_format := ContentFormat, allow_retry := AllowRetry} = BodyOpts} = Task | WorkFlow],
                   sock = Sock,
                   conf = #{host := Host, port := Port}} = Data) ->
    Validitor = fun
        (timeout, _StateData0) ->
            coap_bench_metrics:incr('WAIT_OBSERVE_TIMEOUT');
        (RcvdMsg, StateData0 = #data{observed = Observed}) ->
            case {coap_bench_message:uri_path(RcvdMsg), coap_bench_message:token(RcvdMsg)} of
                {Path0, Token0} when is_binary(Token0) ->
                    Ack = coap_bench_message:make_ack(RcvdMsg, {ok, content}, make_body(BodyOpts),
                            [{observe, 0}, {content_format, ContentFormat}]),
                    send_msg(Sock, Host, Port, Ack),
                    coap_bench_metrics:incr('WAIT_OBSERVE_SUCC'),
                    StateData1 = store_retry_cons(AllowRetry, RcvdMsg, Ack, StateData0),
                    {ok, StateData1#data{observed = Observed#{Path0 => Token0}}};
                {Path0, Token0} ->
                    case check_retry_cons(RcvdMsg, StateData0) of
                        {ok, Ack} ->
                            send_msg(Sock, Host, Port, Ack),
                            {error, ignore};
                        error ->
                            coap_bench_metrics:incr('WAIT_OBSERVE_FAIL'),
                            {error, {observe_path_not_matched, Path0, Token0}}
                    end
            end
        end,
    coap_bench_metrics:incr('WAIT_OBSERVE'),
    {next_state, wait_coap_msg, Data#data{current_task = Task, workflow = WorkFlow, verify_msg = Validitor},
        [{state_timeout, MillSec, wait_msg_timeout}]};

process_task(#data{workflow = [{notify, #{path := Path, content_format := ContentFormat} = BodyOpts} = Task | WorkFlow],
                   sock = Sock,
                   nextmid = MsgId,
                   observed = Observed,
                   conf = #{host := Host, port := Port}} = Data) ->
    case maps:find(Path, Observed) of
        {ok, Token} ->
            coap_bench_metrics:incr('NOTIFY'),
            Notify = coap_bench_message:make_notify(non, Token, MsgId, make_body(BodyOpts), MsgId, ContentFormat),
            send_response(Sock, Host, Port, Notify,
                Data#data{current_task = Task, workflow = WorkFlow}, MsgId);
        error ->
            {stop, {shutdown, {not_observed, Path}}, Data#data{current_task = Task}}
    end;

process_task(#data{workflow = [{pause, #{}} = Task | WorkFlow]} = Data) ->
    coap_bench_metrics:incr('PAUSE'),
    {next_state, pause, Data#data{workflow = WorkFlow, current_task = Task},
        [hibernate]};

process_task(#data{workflow = [{sleep, #{interval := MillSec}} = Task | WorkFlow]} = Data) ->
    {next_state, sleep, Data#data{workflow = WorkFlow, current_task = Task},
        [{state_timeout, MillSec, wakeup}, hibernate]};

process_task(#data{workflow = [{repeat, #{repeat_times := TotalRepeat, work_flow := RepeatFlow}} = Task | WorkFlow]} = Data) ->
    continue_working(
        Data#data{
            current_task = Task,
            workflow = RepeatFlow ++ [{end_repeat, #{work_flow => RepeatFlow, remain => (TotalRepeat-1)}}] ++ WorkFlow
        });

process_task(#data{workflow = [{end_repeat, #{remain := 0, work_flow := _}} = Task | WorkFlow]} = Data) ->
    continue_working(
        Data#data{
            current_task = Task,
            workflow = WorkFlow
        });

process_task(#data{workflow = [{end_repeat, #{remain := RemainNum, work_flow := RepeatFlow}} = Task | WorkFlow]} = Data) ->
    continue_working(
        Data#data{
            current_task = Task,
            workflow = RepeatFlow ++ [{end_repeat, #{work_flow => RepeatFlow, remain => (RemainNum-1)}}] ++ WorkFlow
        });

process_task(#data{workflow = [Task | WorkFlow]} = Data) ->
    logger:error("unknow workflow: ~p, statedata: ~p", [Task, printable_data(Data)]),
    continue_working(Data#data{current_task = Task, workflow = WorkFlow}).

store_retry_cons(true, RcvdMsg, Ack, StateData = #data{allow_retry_cons = ARCons}) ->
    MsgId = coap_bench_message:id(RcvdMsg),
    StateData#data{allow_retry_cons = ARCons#{MsgId => Ack}};
store_retry_cons(false, _RcvdMsg, _Ack, StateData) ->
    StateData.

check_retry_cons(RcvdMsg, #data{allow_retry_cons = ARCons}) ->
    MsgId = coap_bench_message:id(RcvdMsg),
    case maps:find(MsgId, ARCons) of
        {ok, Ack} -> {ok, Ack};
        error -> error
    end.

handle_common_events(StateName, info, {udp, Sock, PeerIP, PeerPortNo, Packet},
             #data{on_unexpected_msg = OnUnexpectedMsg} = Data) ->
    try
        CoapMsg = lwm2m_coap_message_parser:decode(Packet),
        coap_bench_message:incr_counter_rcvd(CoapMsg),
        handle_unexpected_coap_msg(Sock, PeerIP, PeerPortNo, CoapMsg, OnUnexpectedMsg)
    catch
        _:_ ->
            logger:error("received unknown udp message that not expected: ~p, statedata: ~p", [Packet, printable_data(Data)]),
            common_events_state(StateName)
    end;

handle_common_events(StateName, EventType, Event, Data) ->
    logger:warning("received unexpected event: ~p in state: ~p, statedata: ~p", [{EventType, Event}, StateName, printable_data(Data)]),
    common_events_state(StateName).

common_events_state(StateName)
        when StateName =:= pause;
             StateName =:= sleep ->
    {keep_state_and_data, [hibernate]};
common_events_state(_StateName) ->
    keep_state_and_data.

handle_unexpected_coap_msg(_Sock, _PeerIP, _PeerPortNo, CoapMsg, #{action := stop}) ->
    {stop, {shutdown, {unexpected_coap, CoapMsg}}};
handle_unexpected_coap_msg(_Sock, _PeerIP, _PeerPortNo, _CoapMsg, #{action := do_nothing}) ->
    keep_state_and_data;
handle_unexpected_coap_msg(Sock, PeerIP, PeerPortNo, CoapMsg, #{action := empty_ack}) ->
    may_ack_it(Sock, PeerIP, PeerPortNo, CoapMsg),
    keep_state_and_data;
handle_unexpected_coap_msg(Sock, PeerIP, PeerPortNo, CoapMsg, #{action := ack, code := Code}) ->
    Ack = coap_bench_message:make_ack(CoapMsg, Code, <<>>,
                            [{observe, 0}, {content_format, <<"application/octet-stream">>}]),
    send_msg(Sock, PeerIP, PeerPortNo, Ack),
    keep_state_and_data.

terminate(Reason, _State, #data{workflow = [], sock = Sock} = Data) ->
    logger:debug("[~p] terminate: ~p, statedata: ~p", [?MODULE, Reason, printable_data(Data)]),
    sim_trash:take_socket(Sock);
terminate(Reason, _State, #data{sock = Sock} = Data) ->
    logger:error("[~p] terminate with pending tasks, reason: ~p, statedata: ~p", [?MODULE, Reason, printable_data(Data)]),
    sim_trash:take_socket(Sock).

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_request(Sock, Host, Port, CoapMsg, Timeout, StateData, MsgId, Validitor) ->
    case send_msg(Sock, Host, Port, CoapMsg) of
        ok ->
            {next_state, wait_coap_msg,
                StateData#data{
                    nextmid = next_mid(MsgId),
                    verify_msg = Validitor
                },
                [{state_timeout, Timeout, wait_msg_timeout}]};
        {error, Reason} ->
            logger:error("send request failed: ~p, statedata: ~p", [Reason, printable_data(StateData)]),
            {stop, {shutdown, Reason}}
    end.
send_response(Sock, Host, Port, CoapMsg, StateData, MsgId) ->
    case send_msg(Sock, Host, Port, CoapMsg) of
        ok -> continue_working(StateData#data{
                 nextmid = next_mid(MsgId),
                 verify_msg = undefined});
        {error, Reason} ->
            logger:error("send request failed: ~p, statedata: ~p", [Reason, printable_data(StateData)]),
            {stop, {shutdown, Reason}}
    end.

continue_working(StateData) ->
    continue_working(StateData, []).
continue_working(StateData, Actions) ->
    {next_state, working, StateData, [{next_event, internal, continue_workflow} | Actions]}.

trans_workflow(WorkFlow, Vars0) when is_list(WorkFlow) ->
    Vars = [bin(Tk) || Tk <- string:split(string:tokens(str(Vars0), "\n"), ",", all)],
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

str(Str) when is_list(Str) -> Str;
str(Bin) when is_binary(Bin) -> binary_to_list(Bin).

printable_data(#data{
            sockname = SockName,
            nextmid = NextMsgId,
            location = Location,
            observed = Observed,
            workflow = RemainWorkFlow,
            current_task = CurrentTask
        }) ->
    #{sockname => SockName, nextmid => NextMsgId,
      location => Location, observed => Observed,
      current_task => CurrentTask,
      pending_workflow => RemainWorkFlow}.
