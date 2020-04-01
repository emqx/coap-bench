-module(coap_bench_cli).

-export([ run_command/1
        , init_cli/0
        , command_parser/0
        ]).

-define(TAB, ?MODULE).

init_cli() ->
    Parser = cli:parser(
      "coap_bench run",
      "[OPTION]...",
      "A benchmark tool for testing CoAP/LwM2M servers.\n"
      "\n"
      "This tool runs as a local services and send msgs for benchmarking your\n"
      "CoAP or LwM2M server. You could start a cluster of coap_bench to genertate\n"
      "massive amount of simlation concurrent clients!",
      [{h, "-H, --host", "CoAP/LwM2M server hostname or IP address. [delfault: 127.0.0.1]", [{metavar, "HOST"}]},
       {p, "-P, --port", "CoAP/LwM2M server port number. [delfault: 5683]", [{metavar, "PORT"}]},
       {i, "-I, --interval", "Interval of connecting to the CoAP/LwM2M server in ms [delfault: 10]", [{metavar, "INTERVAL"}]},
       {t, "-T, --lifttime", "Lifetime of each LwM2M client in secs [delfault: 60]", [{metavar, "LIFETIME"}]},
       {c, "-C, --client-info-file", "File that contains client info of the LwM2M clients [delfault: do not use client info file]", [{metavar, "LIFETIME"}]}
      ],
      [{version,
        "1.0\n"
        "Copyright (C) 2020 EMQ Technologies Co., Ltd. All rights reserved\n"
        "Written by Shawn <http://github/terry-xiaoyu/>\n"
       }
      ]),
      ets:new(?TAB, [named_table, set, public]),
      ets:insert(?TAB, {parser, Parser}),
      ok.

command_parser() ->
    case ets:lookup(?TAB, parser) of
        [{parser, Parser}] -> Parser;
        [] -> error(cli_not_inited)
    end.

run_command(Args) ->
    handle_parsed(cli:parse_args(Args, command_parser())).

handle_parsed({{ok, print_help}, P}) ->
    cli:print_help(P);
handle_parsed({{ok, print_version}, P}) ->
    cli:print_version(P);
handle_parsed({{ok, Parsed}, _P}) ->
    handle_args(Parsed);
handle_parsed({{error, Err}, P}) ->
    cli:print_error(Err, P).

handle_args({Opts, Args}) ->
    io:format("Options: ~p~n", [Opts]),
    io:format("Args:    ~p~n", [Args]).
