%%%-------------------------------------------------------------------
%% @doc coap_bench top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sim_manager).

-behaviour(supervisor).

-export([ start_link/0
        , start_sim_groups/1
        , stop_sim_groups/0
        , stop_sim_groups/1
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
    R =
     [(fun() ->
        GroupName = group_name(GrpName),
        try
            {ok, _} = supervisor:start_child(?MODULE, [GroupName]),
            sim_group:start_sims(GroupName, parse_workflow(WorkFlow), GroupClientInfos, Conf)
        of
            ok -> io:format("TaskGroup: ~p started~n", [GroupName])
        catch
            Err:Reason:ST ->
                io:format("TaskGroup: ~p failed to start: ~0p~n", [GroupName, {Err,Reason,ST}]),
                {start_group_failed, {GroupName, {Err, Reason}}}
        end
     end)() || #{group_name := GrpName,
                 work_flow := WorkFlow,
                 client_infos := GroupClientInfos} <- coap_bench_client_info:get_client_info()],
    case lists:filter(fun(ok) -> false; (_) -> true end, R) of
        [] -> ok;
        Result -> {error, Result}
    end.

stop_sim_groups() ->
    [supervisor:terminate_child(?MANAGER, GrpPid)
     || {_,GrpPid,_,_} <- supervisor:which_children(?MANAGER)].
stop_sim_groups(GrpName) ->
    supervisor:terminate_child(?MANAGER, erlang:whereis(GrpName)).

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

parse_workflow(WorkFlow) when is_list(WorkFlow) ->
    lists:map(fun do_parse_workflow/1, WorkFlow).

do_parse_workflow(#{<<"task">> := <<"register">>} = Flow) ->
    {register, #{lifetime => maps:get(<<"lifetime">>, Flow, 60),
                 ep => maps:get(<<"ep">>, Flow, <<"$uuid">>),
                 object_links => maps:get(<<"object_links">>, Flow, ?DEFAULT_OBJ_LINKS)
                }};

do_parse_workflow(#{<<"task">> := <<"deregister">>}) ->
    {deregister, #{}};

do_parse_workflow(#{<<"task">> := <<"wait_observe">>, <<"body">> := Body, <<"path">> := Path, <<"timeout">> := Sec}) when is_binary(Body) ->
    {wait_observe, #{path => path_list(Path), body => Body, timeout => Sec}};
do_parse_workflow(#{<<"task">> := <<"wait_observe">>, <<"body">> := #{<<"size">> := Size, <<"type">> := <<"auto_gen_binary">>}, <<"path">> := Path, <<"timeout">> := Sec}) ->
    {wait_observe, #{path => path_list(Path), body => auto_gen_binary, size => Size, timeout => Sec}};

do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := Body, <<"path">> := Path}) when is_binary(Body) ->
    {notify, #{body => Body, path => path_list(Path)}};
do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := #{<<"size">> := Size, <<"type">> := <<"auto_gen_binary">>}, <<"path">> := Path}) ->
    {notify, #{body => auto_gen_binary, size => Size, path => path_list(Path)}};

do_parse_workflow(#{<<"task">> := <<"sleep">>, <<"interval">> := Interval}) when is_integer(Interval) ->
    {sleep, #{interval => Interval}}.

path_list(Path) ->
    string:lexemes(Path, "/ ").
