-module(coap_bench_metrics).

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
    [ 'REGISTER_SUCC'
    , 'REGISTER_FAIL'
    , 'REGISTER'
    , 'NOTIFY'
    , 'DEREGISTER_SUCC'
    , 'DEREGISTER_FAIL'
    , 'DEREGISTER'
    , 'WAIT_OBSERVE'
    , 'WAIT_OBSERVE_SUCC'
    , 'WAIT_OBSERVE_FAIL'
    , 'CON_SENT'
    , 'CON_SEND_FAIL'
    , 'CON_RCVD'
    , 'ACK_SENT'
    , 'ACK_SEND_FAIL'
    , 'ACK_RCVD'
    , 'NON_SENT'
    , 'NON_SEND_FAIL'
    , 'NON_RCVD'
    , 'RST_SENT'
    , 'RST_SEND_FAIL'
    , 'RST_RCVD'
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
