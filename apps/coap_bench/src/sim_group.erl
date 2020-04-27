%%%-------------------------------------------------------------------
%% @doc coap_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sim_group).

-behaviour(supervisor).

-export([ start_link/1
        , start_sims/4
        , resume_sims/1
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

start_sims(GrpName, WorkFlow, ClientInfos, Conf = #{binds := Binds, conn_interval := ConnInterval}) ->
    [begin
        {ok, Sock} = gen_udp:open(0, [{ip, pick(Binds)}, binary, {active, false}, {reuseaddr, false}]),
        {ok, SimPid} = supervisor:start_child(GrpName, [WorkFlow, Vars, Sock, Conf]),
        gen_udp:controlling_process(Sock, SimPid),
        timer:sleep(ConnInterval)
     end|| Vars <- ClientInfos],
    ok.

resume_sims(GroupPid) ->
    [sim_worker:resume(SimPid)
     || {_,SimPid,_,_} <- supervisor:which_children(GroupPid)].

status(GrpName, count) ->
    Status = supervisor:count_children(GrpName),
    proplists:get_value(active, Status, 0).

%% internal functions

pick(List) ->
    lists:nth(rand:uniform(length(List)), List).
