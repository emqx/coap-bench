-module(coap_bench_profiles).

-export([ init/0
        , is_ready/0
        , load_profiles/2
        , get_client_info/0
        , get_workflow/0
        ]).

init() ->
    ets:new(?MODULE, [named_table, set, public]),
    ok.

is_ready() ->
    ets:info(?MODULE, size) > 0.

load_profiles(ClientInfoFile, WorkflowFile) ->
    %% load workflow
    {ok, Fdflow} = file:open(WorkflowFile, [read]),
    Workflow = try iolist_to_binary(load_workflow(Fdflow))
               after file:close(Fdflow)
               end,
    [_ | _] = JsonWorkflow = jsx:decode(Workflow, [return_maps]),
    ets:insert(?MODULE, {workflow, JsonWorkflow}),

    %% load client info and distribute the clients by group
    {ok, FdClient} = file:open(ClientInfoFile, [read]),
    {ClientInfos, ClientNum} =
        try load_clients(FdClient, {[], 0})
        after file:close(FdClient)
        end,
    GroupdClientInfo = tasks_with_grouped_client_infos(get_workflow(), ClientInfos),
    ets:insert(?MODULE, {client_infos, GroupdClientInfo}),
    {ClientNum, length(JsonWorkflow)}.

load_workflow(Fd) ->
    case file:read_line(Fd) of
        eof -> [];
        {ok, Line} ->
            ommit_comments(Line, "//") ++ load_workflow(Fd)
    end.

load_clients(Fd, {Acc, Num}) ->
    case file:read_line(Fd) of
        eof -> {Acc, Num};
        {ok, Line} ->
            case ommit_comments(Line, "#") of
                [] -> load_clients(Fd, {Acc, Num});
                ClientInfo -> load_clients(Fd, {[ClientInfo | Acc], Num+1})
            end
    end.

%% comments are lines begin with "//" or "#" (single line comments) or
%% the texts after "//" or "#" (inline comments)
ommit_comments(Line, CommentToken) ->
    case string:split(Line, CommentToken) of
        [Literals, _Comments] -> string:trim(Literals);
        [Literals] -> string:trim(Literals)
    end.

get_client_info() ->
   case ets:lookup(?MODULE, client_infos) of
       [{_, Infos}] -> Infos;
       [] -> []
   end.

get_workflow() ->
    [{_, Workflow}] =  ets:lookup(?MODULE, workflow),
    Workflow.

tasks_with_grouped_client_infos(Tasks, ClientInfos) ->
    TotalWeight = total_weight(Tasks),
    TotalNum = length(ClientInfos),
    {NewTasks, _, _} = lists:foldl(fun
        (#{<<"group_name">> := GrpName, <<"work_flow">> := WorkFlow} = Flow, {TasksAcc, RemClientInfos0, RemNum0})
                when RemClientInfos0 =:= []; RemNum0 =:= 1 ->
            {[#{group_name => GrpName,
                work_flow => WorkFlow,
                on_unexpected_msg => maps:get(<<"on_unexpected_msg">>, Flow, <<"do_nothing">>),
                client_infos => RemClientInfos0} | TasksAcc], [], 0};
        (#{<<"group_name">> := GrpName, <<"work_flow">> := WorkFlow, <<"weight">> := Weight}  = Flow , {TasksAcc, RemClientInfos0, RemNum0}) ->
            {GroupClientInfos, RemClientInfos} = lists_split(number_by_weight(Weight, TotalWeight, TotalNum), RemClientInfos0),
            {[#{group_name => GrpName,
                work_flow => WorkFlow,
                on_unexpected_msg => maps:get(<<"on_unexpected_msg">>, Flow, <<"do_nothing">>),
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
