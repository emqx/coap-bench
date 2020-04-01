-module(sim_worker).

-behaviour(gen_server).

%% API functions
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(MAX_MESSAGE_ID, 65535). % 16-bit number

-record(state, {sock, workflow = [], conf, nextmid}).

start_link(WorkFlow, Data, Conf) ->
    gen_server:start_link(?MODULE, [WorkFlow, Data, Conf], []).

init([WorkFlow, Data, Conf = #{binds := Binds}]) ->
    self() ! go_workflow,
    {ok, Sock} = gen_udp:open(0, [{ip, pick(Binds)}]),
    {ok, #state{
            workflow = trans_workflow(WorkFlow, Data),
            conf = Conf,
            nextmid = first_mid(),
            sock = Sock
        }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(go_workflow, State = #state{workflow = []}) ->
    {stop, normal, State};
handle_info(go_workflow, State = #state{sock = Sock, workflow = [Flow | WorkFlow], conf = Conf, nextmid = MsgId}) ->
    ok = handle_workflow(Flow, Conf, Sock, MsgId),
    self() ! go_workflow,
    {noreply, State#state{workflow = WorkFlow, nextmid = next_mid(MsgId)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_workflow({register, #{ep := Ep, lifetime := Lifetime}}, #{host := Host, port := Port}, Sock, MsgId) ->
    udp_send(Sock, Host, Port, lwm2m_cmd:make_register(Ep, Lifetime, MsgId)),
    ok;
handle_workflow({_, _}, _Conf, _Sock, _MsgId) ->
    ok.

udp_send(Sock, Host, Port, Data) ->
    ok = gen_udp:send(Sock, Host, Port, Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================

trans_workflow(WorkFlow, Data0) when is_list(WorkFlow) ->
    Data = parse_data(Data0),
    [do_trans_workflow(Flow, Data) || Flow <- WorkFlow].

do_trans_workflow({FlowName, Opts}, Data) when is_map(Opts) ->
    {FlowName, maps:map(fun(_, V) ->
            replace_var(V, Data)
        end, Opts)}.

replace_var(<<"$uuid">>, _Data) -> uuid();
replace_var(<<"$", Int/binary>>, Data) ->
    Idx = binary_to_integer(Int),
    lists:nth(Idx, Data);
replace_var(Var, _Data) -> Var.

uuid() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

parse_data(Data) ->
    [bin(Tk) || Tk <- string:split(Data, ",", all)].

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
