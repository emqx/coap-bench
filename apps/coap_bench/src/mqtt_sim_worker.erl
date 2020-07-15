-module(mqtt_sim_worker).

-behaviour(gen_statem).

-export([start_link/4, resume/1]).

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
            mqtt_client,
            conf = #{},
            workflow :: [],
            on_unexpected_msg :: #{},
            current_task,
            verify_msg :: fun((term()) -> {ok, #data{}} | {error, term()})
        }).

start_link(WorkFlow, OnCoapMsgRcvd, Vars, Conf) ->
    gen_statem:start_link(?MODULE, [WorkFlow, OnCoapMsgRcvd, Vars, Conf], []).

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

init([WorkFlow, OnUnexpectedMsg, Vars, Conf]) ->
    erlang:process_flag(trap_exit, true),
    {ok, working, #data{
            conf = trans_conf(Conf),
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
             #data{verify_msg = Validitor,
                   on_unexpected_msg = OnUnexpectedMsg} = Data) when is_function(Validitor) ->
    try
        lwm2m_coap_message_parser:decode(Packet)
    of
        CoapMsg ->
            coap_bench_message:incr_counter_rcvd(CoapMsg),
            case Validitor(CoapMsg, Data) of
                {ok, StateData} ->
                    continue_working(StateData#data{verify_msg = undefined});
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
    mqtt_bench_metrics:incr('PAUSE', -1),
    continue_working(Data);
pause(EventType, Event, Data) ->
    ?handle_events(EventType, Event, Data).

%% -------------------------------------------
%% Process tasks and tranform the state
%% -------------------------------------------

process_task(#data{workflow = []} = Data) ->
    {stop, {shutdown, workflow_complete}, Data#data{current_task = undefined}};

process_task(#data{workflow = [{connect, ConnOpts} = Task | WorkFlow], conf = Conf} = Data) ->
    {ok, Client} = emqtt:start_link(conn_opts(ConnOpts, Conf)),
    mqtt_bench_metrics:incr('CONNECT'),
    case emqtt:connect(Client) of
        {ok, _Props} ->
            mqtt_bench_metrics:incr('CONNECT_SUCC'),
            continue_working(Data#data{current_task = Task, workflow = WorkFlow, mqtt_client = {Client, ConnOpts}});
        {error, connack_timeout} ->
            mqtt_bench_metrics:incr('CONNECT_TIMEOUT'),
            {stop, {shutdown, connack_timeout}};
        {error, {Reason, _}} ->
            mqtt_bench_metrics:incr('CONNECT_FAIL'),
            {stop, {shutdown, {connack, Reason}}}
    end;

process_task(#data{workflow = [{disconnect, #{timeout := _MillSec}} = Task | WorkFlow], mqtt_client = {Client, ConnOpts}} = Data) ->
    mqtt_bench_metrics:incr('DISCONNECT'),
    case emqtt:disconnect(Client) of
        ok ->
            mqtt_bench_metrics:incr('DISCONNECT_SUCC'),
            continue_working(Data#data{current_task = Task, workflow = WorkFlow, mqtt_client = {Client, ConnOpts}});
        {error, Reason} ->
            mqtt_bench_metrics:incr('DISCONNECT_FAIL'),
            {stop, {shutdown, {disconnect, Reason}}}
    end;

process_task(#data{workflow = [{pause, #{}} = Task | WorkFlow]} = Data) ->
    mqtt_bench_metrics:incr('PAUSE'),
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

handle_common_events(StateName, EventType, {'EXIT',_,{shutdown, _Any}},
    #data{mqtt_client = {_Client, ConnOpts}, conf = Conf, current_task = Task, workflow = WorkFlow} = Data) ->
    {ok, Client} = emqtt:start_link(conn_opts(ConnOpts, Conf)),
    case emqtt:connect(Client) of
        {ok, _Props} ->
            continue_working(Data#data{workflow = [Task | WorkFlow], mqtt_client = {Client, ConnOpts}});
        {error, Reason} ->
            erlang:send_after(3000, self(), {'EXIT', undefined, {shutdown, reconnect}}),
            logger:error("reconnected fail: ~p", [Reason]),
            keep_state_and_data
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

terminate(Reason, _State, #data{workflow = []} = Data) ->
    logger:debug("[~p] terminate: ~p, statedata: ~p", [?MODULE, Reason, printable_data(Data)]);
terminate(Reason, _State, #data{} = Data) ->
    logger:error("[~p] terminate with pending tasks, reason: ~p, statedata: ~p", [?MODULE, Reason, printable_data(Data)]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

continue_working(StateData) ->
    continue_working(StateData, []).
continue_working(StateData, Actions) ->
    {next_state, working, StateData, [{next_event, internal, continue_workflow} | Actions]}.

trans_workflow(WorkFlow, Vars0) when is_list(WorkFlow) ->
    Vars = [bin(Tk) || Tk <- string:split(string:tokens(str(Vars0), "\n"), ",", all)],
    [do_trans_workflow(Flow, Vars) || Flow <- WorkFlow].

do_trans_workflow({FlowName, Opts}, Vars) when is_map(Opts) ->
    {FlowName, coap_bench_utils:replace_map_var(Opts, Vars)}.

trans_conf(Conf) when is_list(Conf) ->
    Hosts = proplists:get_value(hosts, Conf),
    Host = lists:nth(rand:uniform(length(Hosts)), Hosts),
    Conf1 = lists:keydelete(hosts,1, Conf),
    Conf1 ++ [{host, Host}].

bin(Tk) when is_binary(Tk) -> Tk;
bin(Tk) when is_list(Tk) -> list_to_binary(Tk).

str(Str) when is_list(Str) -> Str;
str(Bin) when is_binary(Bin) -> binary_to_list(Bin).

printable_data(#data{
            workflow = RemainWorkFlow,
            current_task = CurrentTask
        }) ->
    #{current_task => CurrentTask,
      pending_workflow => RemainWorkFlow}.

%%%===================================================================
%%% MQTT Options
%%%===================================================================

conn_opts(Conf = #{clientid := ClientId, timeout := _MillSec}, BaseConf) ->
    [{clientid, ClientId}] ++
     opt(username, Conf) ++
     opt(password, Conf) ++
     opt(keepalive, Conf) ++
     [{tcp_opts, [{ip, proplists:get_value(bind, BaseConf)}]}, {force_ping, true}] ++

     BaseConf.

opt(Key, Conf) ->
    case maps:get(Key, Conf, undefined) of
        undefined -> [];
        Value -> [{Key, Value}]
    end.
