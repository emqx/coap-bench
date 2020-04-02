%%%-------------------------------------------------------------------
%% @doc coap_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sim_group).

-behaviour(supervisor).

-export([ start_link/1
        , start_sims/4
        , status/2
        ]).

-export([init/1]).

start_link(GrpName) when is_atom(GrpName) ->
    supervisor:start_link({local, GrpName}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => sim_worker,
          start => {sim_worker, start_link, []},
          restart => temporary,
          shutdown => brutal_kill,
          type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_sims(GrpName, WorkFlow, ClientInfos, Conf = #{conn_interval := ConnInterval}) ->
    [begin
        {ok, _} = supervisor:start_child(GrpName, [WorkFlow, Vars, Conf]),
        timer:sleep(ConnInterval)
     end|| Vars <- ClientInfos],
    ok.

status(GrpName, count) ->
    Status = supervisor:count_children(GrpName),
    proplists:get_value(active, Status, 0).

%% internal functions
