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

-record(state, {workflow = [], data, conf}).

start_link(WorkFlow, Data, Conf) ->
    gen_server:start_link(?MODULE, [WorkFlow, Data, Conf], []).

init([WorkFlow, Data, Conf]) ->
    self() ! start_workflow,
    {ok, #state{workflow = WorkFlow, data = Data, conf = Conf}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_workflow, State = #state{workflow = [Flow | WorkFlow], data = Data, conf = Conf}) ->
    handle_workflow(Flow, Data, Conf),
    {noreply, State#state{workflow = WorkFlow}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_workflow({register, Opts}, Data, Conf) ->
    ok.
