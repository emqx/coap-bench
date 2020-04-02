-module(coap_bench_cli).

-export([ command/1
        , init_cli/0
        , command_parser/1
        ]).

-define(TAB, ?MODULE).

init_cli() ->
    ets:new(?TAB, [named_table, set, public]),
    RunParser = cli:parser(
        "coap_bench run",
        "[OPTION]...",
        "A benchmark tool for testing CoAP/LwM2M servers.\n"
        "\n"
        "This tool runs as a local services and send msgs for benchmarking your\n"
        "CoAP or LwM2M server. You could start a cluster of coap_bench to genertate\n"
        "massive amount of simlation concurrent clients!\n"
        "\n",
        [
            {h, "-H, --host", "CoAP/LwM2M server hostname or IP address. [delfault: 127.0.0.1]", [{metavar, "HOST"}]},
            {p, "-P, --port", "CoAP/LwM2M server port number. [delfault: 5683]", [{metavar, "PORT"}]},
            {i, "-I, --connect-interval", "Interval of connecting to the CoAP/LwM2M server in ms, can be used to control the connectoin establish speed. [delfault: 10]", [{metavar, "INTERVAL"}]},
            {b, "-B, --bind", "Local IP address to bind on. Separated by commas if there're many IP addresses to be bind. [delfault: 127.0.0.1]", [{metavar, "LOCAL_IP_ADDRs"}]}
        ],
        [{version,
            "0.1.0\n"
            "Copyright (C) 2020 EMQ Technologies Co., Ltd. All rights reserved\n"
            "Written by Shawn <http://github/terry-xiaoyu/>\n"
        }]
    ),

    StatusParser = cli:parser(
        "coap_bench status",
        "",
        "Show status of the coap_bench\n",
        [{s, "-S, --sims", "Show status of the simulators, e.g. how many sims in each group", [flag]},
         {m, "-M, --metrics", "Show metrics, e.g. how many registers have been sent and the success ratio", [flag]}
        ],
        []
    ),

    LoadParser = cli:parser(
        "coap_bench load",
        "<client-info-file> <workflow-file>",
        "Load the file into memory."
        "The <client-info-file> should be a csv file that contains connection params of each client.",
        [], []
    ),

    ets:insert(?TAB, {run, RunParser}),
    ets:insert(?TAB, {load, LoadParser}),
    ets:insert(?TAB, {status, StatusParser}),
    ok.

command_parser(ParserType) ->
    case ets:lookup(?TAB, ParserType) of
        [{_, Parser}] -> Parser;
        [] -> error(cli_not_inited)
    end.

command([Type | Args]) ->
    CmdType = list_to_atom(Type),
    case cli:parse_args(Args, command_parser(CmdType)) of
        {{ok, print_help}, P} ->
            cli:print_help(P);
        {{ok, print_version}, P} ->
            cli:print_version(P);
        {{ok, Parsed}, _P} ->
            handle_args(CmdType, Parsed);
        {{error, Err}, P} ->
            cli:print_error(Err, P)
    end.

handle_args(status, {Opts, _Args}) ->
    proplists:get_value(s, Opts, true) andalso print_status(),
    proplists:get_value(m, Opts, true) andalso print_metrics();

handle_args(run, {Opts, []}) ->
    case coap_bench_client_info:is_ready() of
        true ->
            Conf = parse_conf(Opts),
            io:format("Start test with Conf: ~p~n", [Conf]),
            sim_manager:start_sim_groups(Conf);
        false ->
            io:format("Not initialized. Please do 'coap_bench load <client-info-file> <workflow-file>' first!~n")
    end;
handle_args(run, {_, _}) ->
    io:format("coap_bench run do not accept arguments!~n");

handle_args(load, {_, [Filename, WorkflowFile]}) ->
    io:format("loading client info file: ~p into memory~n", [Filename]),
    io:format("loading workflow file: ~p into memory~n", [WorkflowFile]),
    coap_bench_client_info:load_client_info(file, Filename),
    coap_bench_client_info:load_workflow(file, WorkflowFile);
handle_args(load, {_, _}) ->
    io:format("coap_bench load only accept 2 arguments!~n").

print_status() ->
    case sim_manager:status(count) of
        0 ->
            io:format("No Running Task Groups!~n");
        GrpCount ->
            io:format("~60..=s~n", [""]),
            io:format("Total ~w Running Task Groups~n", [GrpCount]),
            io:format("~60..=s~n", [""]),
            io:format("~-48s~s~n", ["TaskGroup", "RunningSims"]),
            io:format("~60..-s~n", [""]),
            [io:format("~-48s: ~p~n", [GrpName, SimCount])
            || {GrpName, SimCount} <- sim_manager:status(list)]
    end.

print_metrics() ->
    ok.

parse_conf(Opts) ->
    {ok, Host} = inet:parse_address(proplists:get_value(h, Opts, "127.0.0.1")),
    Port = list_to_integer(proplists:get_value(p, Opts, "5683")),
    Binds = [begin {ok, IP} = inet:parse_address(IPAddr), IP end
            || IPAddr <- string:tokens(proplists:get_value(b, Opts, "127.0.0.1"), ", ")],
    ConnInterval = list_to_integer(proplists:get_value(i, Opts, "10")),
    #{host => Host, port => Port,
      binds => Binds, conn_interval => ConnInterval}.
