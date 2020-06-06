-module(mqtt_bench_metrics).

-compile({parse_transform, counter_trans}).
-compile({no_auto_import, [get/1]}).

-on_load(init/0).

-export([ init/0
        , incr/1
        , incr/2
        , get/1
        , get_all/0
        ]).

-counters(
    [ 'CONNECT'
    , 'CONNECT_SUCC'
    , 'CONNECT_FAIL'
    , 'CONNECT_TIMEOUT'
    , 'DISCONNECT'
    , 'DISCONNECT_SUCC'
    , 'DISCONNECT_FAIL'
    , 'DISCONNECT_TIMEOUT'
    , 'PAUSE'
    ]).

%% Magic functions
-define(SIZE, '@size'()).
-define(INDEX(Name), '@index'(Name)).
-define(COUNTERS, '@counters'()).

init() ->
    CounterRef = counters:new(?SIZE, [write_concurrency]),
    persistent_term:put(?MODULE, CounterRef).

ref() ->
    persistent_term:get(?MODULE).

incr(CounterName) ->
    incr(CounterName, 1).

incr(CounterName, Incr) when is_integer(Incr) ->
    counters:add(ref(), ?INDEX(CounterName), Incr).

all() -> ?COUNTERS.

get(CounterName) ->
    counters:get(ref(), ?INDEX(CounterName)).

get_all() ->
    [{CounterName, get(CounterName)} || CounterName <- all()].
