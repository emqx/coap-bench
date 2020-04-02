-module(coap_bench_client_info).

-export([init/0, is_ready/0, load_client_info/2, load_workflow/2,
        get_client_info/0, get_workflow/0]).

init() ->
    ets:new(?MODULE, [named_table, set, public]),
    ok.

is_ready() ->
    ets:info(?MODULE, size) > 0.

load_workflow(file, FileName) ->
    {ok, Workflow} = file:read_file(FileName),
    ets:insert(?MODULE, {workflow, jsx:decode(Workflow, [return_maps])}),
    ok.

load_client_info(file, FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try load_all_lines(Device)
      after file:close(Device)
    end,
    ok.

load_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line when Line =/= "" ->
            load_line(Line),
            load_all_lines(Device)
    end,
    ok.

load_line(ClientInfo) ->
    ets:insert(?MODULE,
        {client_infos, [ClientInfo|coap_bench_client_info:get_client_info()]}),
    ok.

get_client_info() ->
   case ets:lookup(?MODULE, client_infos) of
       [{_, Infos}] -> Infos;
       [] -> []
   end.

get_workflow() ->
    [{_, Workflow}] =  ets:lookup(?MODULE, workflow),
    Workflow.
