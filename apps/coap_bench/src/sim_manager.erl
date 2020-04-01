%%%-------------------------------------------------------------------
%% @doc coap_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sim_manager).

-behaviour(supervisor).

-export([ start_link/0
        , start_sim_groups/3
        , status/1
        ]).

-export([init/1]).

-define(MANAGER, ?MODULE).

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

start_sim_groups(Tasks, DataSet, Conf) ->
    [begin
        {ok, _} = supervisor:start_child(?MODULE, [group_name(GrpName)]),
        ok = sim_group:start_sims(group_name(GrpName), parse_workflow(WorkFlow), GroupDataSet, Conf)
     end
    || #{group_name := GrpName,
         work_flow := WorkFlow,
         dataset := GroupDataSet} <- tasks_with_grouped_dataset(Tasks, DataSet)].

status(count) ->
    supervisor:count_children(?MANAGER);
status(list) ->
    supervisor:which_children(?MANAGER).

%% internal functions

group_name(GrpName) ->
    list_to_atom("sim_group_" ++ binary_to_list(GrpName)).

tasks_with_grouped_dataset(Tasks, DataSet) ->
    {TotalWeight, TotalNum} = total_weight(Tasks),
    lists:foldl(fun
        (#{<<"group_name">> := GrpName, <<"work_flow">> := WorkFlow}, {TasksAcc, RemDataSet0, RemNum0})
                when RemDataSet0 =:= []; RemNum0 =:= 1 ->
            {[#{group_name => GrpName,
                work_flow => WorkFlow,
                dataset => RemDataSet0} | TasksAcc], [], 0};
        (#{<<"group_name">> := GrpName, <<"work_flow">> := WorkFlow, weight := Weight}, {TasksAcc, RemDataSet0, RemNum0}) ->
            {GroupDataSet, RemDataSet} = lists_split(number_by_weight(Weight, TotalWeight, TotalNum), RemDataSet0),
            {[#{group_name => GrpName,
                work_flow => WorkFlow,
                dataset => GroupDataSet} | TasksAcc],
             RemDataSet, RemNum0-1}
        end, {[], DataSet, TotalNum}, Tasks).

total_weight(Tasks) ->
    lists:foldl(fun(#{weight := Weight}, {WeightAcc, Num}) when is_integer(Weight) ->
            {WeightAcc + Weight, Num + 1}
        end, {0, 0}, Tasks).

number_by_weight(Weight, TotalWeight, TotalNum) ->
    ceil((Weight / TotalWeight) * TotalNum).

lists_split(Len, List) ->
    try lists:split(Len, List)
    catch
        error:badarg ->
            case length(Len) > List of
                true -> {List, []};
                false -> error({invalid_split, Len, List})
            end
    end.

parse_workflow(WorkFlow) when is_list(WorkFlow) ->
    lists:map(fun do_parse_workflow/1, WorkFlow).

do_parse_workflow(#{<<"cmd">> := <<"register">>} = Flow) ->
    case maps:get(<<"lifetime">>, Flow, 60) of
        Lifetime when is_integer(Lifetime) ->
            {register, #{lifetime => Lifetime}};
        Lifetime ->
            error(invalid, Lifetime)
    end;
do_parse_workflow(#{<<"cmd">> := <<"deregister">>}) ->
    {deregister, #{}};
do_parse_workflow(#{<<"cmd">> := <<"sleep">>, <<"interval">> := Interval}) when is_integer(Interval) ->
    {sleep, #{interval => Interval}}.
