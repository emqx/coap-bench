%%%-------------------------------------------------------------------
%% @doc coap_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sim_group).

-behaviour(supervisor).

-export([ start_link/2
        , start_sims/6
        , resume_sims/1
        , status/2
        ]).

-export([init/1]).

start_link(Type, GrpName) when is_atom(Type), is_atom(GrpName) ->
    supervisor:start_link({local, GrpName}, ?MODULE, [Type]).

init([Type]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    Module = case Type of lwm2m -> coap_sim_worker; mqtt -> mqtt_sim_worker end,
    ChildSpecs = [
        #{id => Module,
          start => {Module, start_link, []},
          restart => temporary,
          shutdown => brutal_kill,
          type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_sims(lwm2m, GrpName, WorkFlow, OnUnexpectedMsg, ClientInfos, Conf = #{binds := Binds, conn_interval := ConnInterval}) ->
    [begin
        {ok, Sock} = gen_udp:open(0, [{ip, pick(Binds)}, binary, {active, false}, {reuseaddr, false}]),
        {ok, SimPid} = supervisor:start_child(GrpName, [WorkFlow, OnUnexpectedMsg, Vars, Sock, Conf]),
        gen_udp:controlling_process(Sock, SimPid),
        timer:sleep(ConnInterval)
     end|| Vars <- ClientInfos],
    ok;

start_sims(mqtt, GrpName, WorkFlow, OnUnexpectedMsg, ClientInfos, Conf = #{conn_interval := ConnInterval}) ->
    [begin
        {ok, _SimPid} = supervisor:start_child(GrpName, [WorkFlow, OnUnexpectedMsg, Vars, mqtt_conf(Conf)]),
        timer:sleep(ConnInterval)
     end|| Vars <- ClientInfos],
    ok.

resume_sims(GroupPid) ->
    [coap_sim_worker:resume(SimPid)
     || {_,SimPid,_,_} <- supervisor:which_children(GroupPid)].

status(GrpName, count) ->
    Status = supervisor:count_children(GrpName),
    proplists:get_value(active, Status, 0).

%% internal functions

pick(List) ->
    lists:nth(rand:uniform(length(List)), List).

mqtt_conf(#{host := Host, port := Port, binds := Binds}) ->
    [{host, Host}, {port, Port}, {bind, pick(Binds)}].
