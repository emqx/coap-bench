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
        , resume_sim_groups/0
        ]).

-export([init/1]).

-define(MANAGER, ?MODULE).

-define(DEFAULT_OBJ_LINKS, <<"</>;rt=\"oma.lwm2m\",</1/0>,</3/0>">>).
-define(REQ_TIMEOUT, 30). %% 30 seconds

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
    [spawn(
    fun() ->
        GroupName = group_name(GrpName),
        try
            case supervisor:start_child(?MODULE, [GroupName]) of
                {error,{already_started, _}} ->
                    error({test_group_already_started, GroupName});
                {ok, _} -> ok
            end,
            sim_group:start_sims(GroupName,
                parse_workflow(WorkFlow),
                on_unexpected_msg(OnUnexpectedMsg),
                GroupClientInfos, Conf)
        of
            ok -> logger:info("TaskGroup: ~p started", [GroupName])
        catch
            Err:Reason:ST ->
                logger:error("TaskGroup: ~p failed to start: ~0p", [GroupName, {Err,Reason,ST}]),
                timer:sleep(100),
                {start_group_failed, {GroupName, {Err, Reason}}}
        end
    end)
    || #{group_name := GrpName,
         work_flow := WorkFlow,
         on_unexpected_msg := OnUnexpectedMsg,
         client_infos := GroupClientInfos
        } <- coap_bench_profiles:get_client_info()],
    ok.

stop_sim_groups() ->
    [supervisor:terminate_child(?MANAGER, GrpPid)
     || {_,GrpPid,_,_} <- supervisor:which_children(?MANAGER)].
stop_sim_groups(GrpName) ->
    supervisor:terminate_child(?MANAGER, erlang:whereis(GrpName)).

resume_sim_groups() ->
    [sim_group:resume_sims(GrpPid)
     || {_,GrpPid,_,_} <- supervisor:which_children(?MANAGER)].

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
                 object_links => maps:get(<<"object_links">>, Flow, ?DEFAULT_OBJ_LINKS),
                 timeout => maps:get(<<"timeout">>, Flow, ?REQ_TIMEOUT)
                }};

do_parse_workflow(#{<<"task">> := <<"deregister">>} = Flow) ->
    {deregister, #{timeout => maps:get(<<"timeout">>, Flow, ?REQ_TIMEOUT)}};

do_parse_workflow(#{<<"task">> := <<"wait_observe">>, <<"body">> := Body, <<"path">> := Path, <<"timeout">> := Sec} = Flow) when is_binary(Body) ->
    {wait_observe, #{path => path_list(Path), body => Body, timeout => Sec, content_format => content_format(Flow)}};
do_parse_workflow(#{<<"task">> := <<"wait_observe">>, <<"body">> := #{<<"size">> := Size, <<"type">> := <<"auto_gen_binary">>}, <<"path">> := Path, <<"timeout">> := Sec} = Flow) ->
    {wait_observe, #{path => path_list(Path), body => auto_gen_binary, size => Size, timeout => Sec, content_format => content_format(Flow)}};

do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := Body, <<"path">> := Path} = Flow) when is_binary(Body) ->
    {notify, #{body => Body, path => path_list(Path), content_format => content_format(Flow)}};
do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := #{<<"size">> := Size, <<"type">> := <<"auto_gen_binary">>}, <<"path">> := Path} = Flow) ->
    {notify, #{body => auto_gen_binary, size => Size, path => path_list(Path), content_format => content_format(Flow)}};
do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := #{<<"type">> := <<"base64">>, <<"payload">> := Payload}, <<"path">> := Path} = Flow) ->
    {notify, #{body => base64, payload => Payload, path => path_list(Path), content_format => content_format(Flow)}};
do_parse_workflow(#{<<"task">> := <<"notify">>, <<"body">> := #{<<"type">> := <<"hexstring">>, <<"payload">> := Payload}, <<"path">> := Path} = Flow) ->
    {notify, #{body => hexstring, payload => Payload, path => path_list(Path), content_format => content_format(Flow)}};

do_parse_workflow(#{<<"task">> := <<"pause">>}) ->
    {pause, #{}};

do_parse_workflow(#{<<"task">> := <<"sleep">>, <<"interval">> := Interval}) ->
    {sleep, #{interval => interval(Interval)}};

do_parse_workflow(#{<<"task">> := <<"repeat">>, <<"repeat_times">> := RepeatNum, <<"work_flow">> := WorkFlow}) ->
    true = (is_integer(RepeatNum) andalso RepeatNum > 0),
    {repeat, #{
        repeat_times => RepeatNum,
        work_flow => parse_workflow(WorkFlow)
    }}.

content_format(Flow) ->
    maps:get(<<"content_format">>, Flow, <<"application/octet-stream">>).

path_list(Path) ->
    string:lexemes(Path, "/ ").

interval(infinity) -> infinity;
interval(Sec) when is_integer(Sec) -> timer:seconds(Sec);
interval(Interval) when is_binary(Interval) ->
    case re:run(Interval,"(\\d+)(ms|s|m|h|d)",[{capture,all_but_first,list}]) of
        nomatch -> error({invalid_interval, Interval});
        {match,[Num,"ms"]} ->
            list_to_integer(Num);
        {match,[Num,"s"]} ->
            timer:seconds(list_to_integer(Num));
        {match,[Num,"m"]} ->
            timer:seconds(list_to_integer(Num)) * 60;
        {match,[Num,"h"]} ->
            timer:seconds(list_to_integer(Num)) * 60 * 60;
        {match,[Num,"d"]} ->
            timer:seconds(list_to_integer(Num)) * 60 * 60 * 24
    end.

on_unexpected_msg(<<"stop">>) -> #{action => stop};
on_unexpected_msg(<<"do_nothing">>) -> #{action => do_nothing};
on_unexpected_msg(<<"empty_ack">>) -> #{action => empty_ack};
on_unexpected_msg(#{<<"ack">> := Code}) -> #{action => ack, code => coap_bench_message:coap_code(Code)}.
