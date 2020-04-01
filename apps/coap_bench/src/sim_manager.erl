%%%-------------------------------------------------------------------
%% @doc coap_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sim_manager).

-behaviour(supervisor).

-export([ start_link/0
        , start_sim_group/1
        , status/1
        ]).

-export([init/1]).

-define(MANAGER, ?MODULE).
-define(GROUP, sim_group).

start_link() ->
    supervisor:start_link({local, ?MANAGER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => sim_group,
          start => {sim_group, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => supervisor}
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_sim_group(#{<<"group_name">> := GrpName, <<"work_flow">> := WorkFlowList}) ->
    [{ok, _} = supervisor:start_child(group_name(GrpName), [WorkFlow])
     || WorkFlow <- WorkFlowList].

status(count) ->
    supervisor:count_children(?MANAGER);
status(list) ->
    supervisor:which_children(?MANAGER).

%% internal functions

group_name(GrpName) ->
    list_to_atom(atom_to_list(?GROUP) ++ "_" ++ binary_to_list(GrpName)).
