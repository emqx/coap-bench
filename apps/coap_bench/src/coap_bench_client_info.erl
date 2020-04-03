-module(coap_bench_client_info).

-export([init/0, is_ready/0, load_client_info/2, load_workflow/2,
        get_client_info/0, get_workflow/0, load_all_lines/1]).

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
    ClientInfos = load_line(Device, []),
    ets:insert(?MODULE, {client_infos, ClientInfos}).

load_line(Device, Acc) ->
    case file:read_line(Device) of
        eof -> Acc;
        {ok, ClientInfo} ->
           load_line(Device, [ClientInfo | Acc])
    end.

get_client_info() ->
   case ets:lookup(?MODULE, client_infos) of
       [{_, Infos}] -> Infos;
       [] -> []
   end.

get_workflow() ->
    [{_, Workflow}] =  ets:lookup(?MODULE, workflow),
    Workflow.
