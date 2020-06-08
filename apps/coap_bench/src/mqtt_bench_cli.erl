-module(mqtt_bench_cli).

-export([ command/1
        ]).

-define(RUN_OPTS,
        [{help, undefined, "help", boolean,
          "help information"},
         {host, $h, "host", {string, "localhost"},
          "mqtt server hostname or IP address"},
         {port, $p, "port", {integer, 1883},
          "mqtt server port number"},
         {version, $V, "version", {integer, 5},
          "mqtt protocol version: 3 | 4 | 5"},
         {count, $c, "count", {integer, 200},
          "max count of clients"},
         {startnumber, $n, "startnumber", {integer, 0}, "start number"},
         {'connect-interval', $i, "connect-interval", {integer, 10},
          "interval of connecting to the broker"},
         {bind, $b, "bind", string,
          "local ipaddress or interface address"}
        ]).

-define(CLEAR_OPTS,
        [{help, undefined, "help", boolean,
          "help information"},
         {group, $g, "group", string,
          "task group name. If not specified, all of the groups will be stopped"}
        ]).

-define(STATUS_OPTS,
        [{help, undefined, "help", boolean,
          "help information"},
         {sims, $s, "sims", boolean,
          "show status of the simulators, e.g. how many sims in each group"},
         {metrics, $m, "metrics", boolean,
          "show metrics, e.g. how many registers have been sent and the success ratio"}
        ]).

-define(TAB, ?MODULE).
-define(IDX_SENT, 1).
-define(IDX_RECV, 2).

command(["run"|Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?RUN_OPTS, Argv),
    ok = maybe_help(run, Opts),
    case coap_bench_profiles:is_ready() of
        true ->
            Conf = parse_conf(Opts),
            case proplists:get_value(force, Opts, false) of
                true ->
                    sim_manager:stop_sim_groups(),
                    io:format("Force start test with conf: ~p~n", [Conf]);
                false -> io:format("Start test with conf: ~p~n", [Conf])
            end,
            ok = sim_manager:start_sim_groups(mqtt, Conf);
        false ->
            io:format("Not initialized. Please do 'mqtt_bench load <client-info-file> <workflow-file>' first!~n")
    end;

command(["resume"|_]) ->
    sim_manager:resume_sim_groups();

command(["clear"|Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?CLEAR_OPTS, Argv),
    ok = maybe_help(clear, Opts),
    case proplists:get_value(group, Opts, all) of
        all ->
            io:format("Stop all task groups~n"),
            sim_manager:stop_sim_groups();
        GrpName ->
            Name = list_to_atom(GrpName),
            io:format("Stop task group: ~p~n", [Name]),
            sim_manager:stop_sim_groups(Name)
    end;

command(["status"|Argv]) ->
    {ok, {Opts, _Args}} = getopt:parse(?STATUS_OPTS, Argv),
    ok = maybe_help(status, Opts),
    case sim_manager:status(count) of
        0 ->
            io:format("No Running Task Groups!~n");
        GrpCount ->
            case {proplists:get_value(sims, Opts, false), proplists:get_value(metrics, Opts, false)} of
                {false, false} -> print_status(GrpCount),print_metrics();
                {true, true} -> print_status(GrpCount),print_metrics();
                {true, false} -> print_status(GrpCount);
                {false, true} -> print_metrics()
             end
    end;

command(["load", ClientInfoFile, WorkflowFile]) ->
    {ClientCount, GroupsCount} = coap_bench_profiles:load_profiles(ClientInfoFile, WorkflowFile),
    io:format("~nLoading profiles into memory...~n~n"
              "~p clients loaded from client info file:\t~p~n"
              "~p groups  loaded from workflow file:\t~p~n",
              [ClientCount, ClientInfoFile,
               GroupsCount, WorkflowFile]);
command(["load"| _]) ->
    io:format("Usage: ./mqtt_bench load <client-info>.csv <workflow>.json~n").

print_status(GrpCount) ->
    io:format("~60..=s~n", [""]),
    io:format("~-48s~s~n", ["RunningTaskGroups", "RunningSims"]),
    io:format("~60..-s~n", [""]),
    SimCountTotal =
        lists:foldl(fun({GrpName, SimCount}, CountAcc) ->
            io:format("~-48s: ~p~n", [GrpName, SimCount]),
            CountAcc + SimCount
        end, 0, sim_manager:status(list)),
    io:format("~n"),
    io:format("TaskGroups Total: ~w, Sims Total: ~w~n", [GrpCount, SimCountTotal]),
    io:format("~60..=s~n", [""]).

print_metrics() ->
    io:format("~60..=s~n", [""]),
    io:format("~-48s~s~n", ["Metrics", "Value"]),
    io:format("~60..-s~n", [""]),
    lists:foreach(fun({Metric, Value}) ->
            io:format("~-48s: ~p~n", [Metric, Value])
        end, mqtt_bench_metrics:get_all()),
    io:format("~60..=s~n", [""]).

parse_conf(Opts) ->
    Hosts = [begin {ok, IP} = inet:parse_address(IPAddr), IP end
            || IPAddr <- string:tokens(proplists:get_value(host, Opts, "127.0.0.1"), ", ")],
    Port = to_integer(proplists:get_value(port, Opts, "1883")),
    Binds = [begin {ok, IP} = inet:parse_address(IPAddr), IP end
            || IPAddr <- string:tokens(proplists:get_value(bind, Opts, "127.0.0.1"), ", ")],
    ConnInterval = to_integer(proplists:get_value('connect-interval', Opts, "10")),
    #{hosts => Hosts, port => Port,
      binds => Binds, conn_interval => ConnInterval}.

maybe_help(PubSub, Opts) ->
    case proplists:get_value(help, Opts) of
        true ->
            usage(PubSub),
            halt(0);
        _ -> ok
    end.

usage(PubSub) ->
    getopt:usage(
        case PubSub of
            run -> ?RUN_OPTS;
            clear -> ?CLEAR_OPTS;
            status -> ?STATUS_OPTS
        end,
        "./mqtt_bench " ++ atom_to_list(PubSub)).

to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(I) when is_integer(I) -> I.
