%%%-------------------------------------------------------------------
%% @doc coap_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sim_manager).

-behaviour(supervisor).

-export([ start_link/0
        , start_sim_groups/1
        , status/1
        ]).

-export([init/1]).

-define(MANAGER, ?MODULE).

-define(DEFAULT_OBJ_LINKS, <<"</>;rt=\"oma.lwm2m\",</1/0>,</3/0>">>).

start_link() ->
    supervisor:start_link({local, ?MANAGER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 20,
                 period => 3},
    ChildSpecs = [
        #{id => sim_group,
          start => {sim_group, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => supervisor}
    ],
    {ok, {SupFlags, ChildSpecs}}.

start_sim_groups(Conf) ->
    Tasks = coap_bench_client_info:get_workflow(),
    ClientInfos = coap_bench_client_info:get_client_info(),
    [spawn(fun() ->
        {ok, _} = supervisor:start_child(?MODULE, [group_name(GrpName)]),
        ok = sim_group:start_sims(group_name(GrpName), parse_workflow(WorkFlow), GroupClientInfos, Conf)
     end) || #{group_name := GrpName,
               work_flow := WorkFlow,
               client_infos := GroupClientInfos} <- tasks_with_grouped_client_infos(Tasks, ClientInfos)].

status(count) ->
    Status = supervisor:count_children(?MANAGER),
    proplists:get_value(active, Status, 0);
status(list) ->
    [begin
        {registered_name, Name} = erlang:process_info(GrpPid, registered_name),
        {Name, sim_group:status(GrpPid, count)}
     end || {_,GrpPid,_,_} <- supervisor:which_children(?MANAGER)].

%% internal functions

group_name(GrpName) ->
    list_to_atom("sim_group_" ++ binary_to_list(GrpName)).

tasks_with_grouped_client_infos(Tasks, ClientInfos) ->
    TotalWeight = total_weight(Tasks),
    TotalNum = length(ClientInfos),
    {NewTasks, _, _} = lists:foldl(fun
        (#{<<"group_name">> := GrpName, <<"work_flow">> := WorkFlow}, {TasksAcc, RemClientInfos0, RemNum0})
                when RemClientInfos0 =:= []; RemNum0 =:= 1 ->
            {[#{group_name => GrpName,
                work_flow => WorkFlow,
                client_infos => RemClientInfos0} | TasksAcc], [], 0};
        (#{<<"group_name">> := GrpName, <<"work_flow">> := WorkFlow, <<"weight">> := Weight}, {TasksAcc, RemClientInfos0, RemNum0}) ->
            {GroupClientInfos, RemClientInfos} = lists_split(number_by_weight(Weight, TotalWeight, TotalNum), RemClientInfos0),
            {[#{group_name => GrpName,
                work_flow => WorkFlow,
                client_infos => GroupClientInfos} | TasksAcc],
             RemClientInfos, RemNum0-1}
        end, {[], ClientInfos, TotalNum}, Tasks),
    NewTasks.

total_weight(Tasks) ->
    lists:foldl(fun(#{<<"weight">> := Weight}, WeightAcc) when is_integer(Weight) ->
            WeightAcc + Weight
        end, 0, Tasks).

number_by_weight(Weight, TotalWeight, TotalNum) ->
    ceil((Weight / TotalWeight) * TotalNum).

lists_split(Len, List) ->
    try lists:split(Len, List)
    catch
        error:badarg ->
            case Len > length(List) of
                true -> {List, []};
                false -> error({invalid_split, Len, List})
            end
    end.

parse_workflow(WorkFlow) when is_list(WorkFlow) ->
    lists:map(fun do_parse_workflow/1, WorkFlow).

do_parse_workflow(#{<<"task">> := <<"register">>} = Flow) ->
    {register, #{lifetime => maps:get(<<"lifetime">>, Flow, 60),
                 ep => maps:get(<<"ep">>, Flow, <<"$uuid">>),
                 object_links => maps:get(<<"object_links">>, Flow, ?DEFAULT_OBJ_LINKS)
                }};

do_parse_workflow(#{<<"task">> := <<"deregister">>}) ->
    {deregister, #{}};

do_parse_workflow(#{<<"task">> := <<"wait_observe">>, <<"path">> := Path, <<"timeout">> := Sec}) ->
    {wait_observe, #{path => path_list(Path), timeout => Sec}};

do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := Body, <<"path">> := Path}) when is_binary(Body) ->
    {notify, #{body => Body, path => path_list(Path)}};
do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := #{<<"size">> := Size, <<"type">> := <<"auto_gen_binary">>}, <<"path">> := Path}) ->
    {notify, #{body => auto_gen_binary, size => Size, path => path_list(Path)}};

do_parse_workflow(#{<<"task">> := <<"sleep">>, <<"interval">> := Interval}) when is_integer(Interval) ->
    {sleep, #{interval => Interval}}.

path_list(Path) ->
    string:lexemes(Path, "/ ").
