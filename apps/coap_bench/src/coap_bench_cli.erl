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
        "massive amount of simlation concurrent clients!",
        [
            {h, "-H, --host", "CoAP/LwM2M server hostname or IP address. [delfault: 127.0.0.1]", [{metavar, "HOST"}]},
            {p, "-P, --port", "CoAP/LwM2M server port number. [delfault: 5683]", [{metavar, "PORT"}]},
            {i, "-I, --interval", "Interval of connecting to the CoAP/LwM2M server in ms, can be used to control the connectoin establish speed. [delfault: 10]", [{metavar, "INTERVAL"}]},
            {c, "-C, --client-info-file", "File that contains client info of the LwM2M clients [delfault: do not use client info file]", [{metavar, "LIFETIME"}]},
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
        [{s, "-S, --sims", "Show status of the simulators", [flag]}
        ],
        []
    ),

    ets:insert(?TAB, {run, RunParser}),
    ets:insert(?TAB, {run, StatusParser}),
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

handle_args(status, {Opts, Args}) ->
    io:format("Options: ~p~n", [Opts]),
    io:format("Args:    ~p~n", [Args]);

handle_args(run, {Opts, Args}) ->
    io:format("Options: ~p~n", [Opts]),
    io:format("Args:    ~p~n", [Args]),
    Conf = parse_conf(Opts),
    Tasks = read_tasks(),
    DataSet = read_dataset(),
    sim_manager:start_sim_groups(Tasks, DataSet, Conf).

parse_conf(Opts) ->
    {ok, Host} = inet:parse_address(proplists:get_value(h, Opts, "127.0.0.1")),
    Port = list_to_integer(proplists:get_value(p, Opts, "5683")),
    Binds = [begin {ok, IP} = inet:parse_address(IPAddr), IP end
            || IPAddr <- string:tokens(proplists:get_value(b, Opts, "127.0.0.1"), ", ")],
    #{host => Host, port => Port, binds => Binds}.

read_tasks() ->
    [#{ <<"group_name">> => <<"register_only">>,
        <<"weight">> => 1,
        <<"work_flow">> => [
            #{
                <<"cmd">> => <<"register">>,
                <<"lifetime">> => 60
            },
            #{
                <<"cmd">> => <<"sleep">>,
                <<"interval">> => 30
            },
            #{
                <<"cmd">> => <<"deregister">>
            }
        ]
     },
     #{ <<"group_name">> => <<"notify">>,
        <<"weight">> => 1,
        <<"work_flow">> => [
            #{
                <<"cmd">> => <<"register">>,
                <<"lifetime">> => 60
            },
            #{
                <<"cmd">> => <<"sleep">>,
                <<"interval">> => 30
            },
            #{
                <<"cmd">> => <<"notify">>,
                <<"body">> =>
                    #{<<"type">> => <<"auto_gen_binary">>,
                      <<"size">> => 12}
            },
            #{
                <<"cmd">> => <<"deregister">>
            }
        ]
     }
    ].

read_dataset() ->
    ["874625413356234,60,beifnigbabef",
     "874625413356235,120,exnaoeitbqid"
    ].