-module(coap_bench_utils).

-export([replace_map_var/2]).

replace_map_var(Map, Data) ->
    maps:map(fun(_, V) ->
            replace_var(V, Data)
        end, Map).

replace_var(<<"$uuid">>, _Data) -> uuid();
replace_var(<<"$", Int/binary>>, Data) ->
    Idx = binary_to_integer(Int),
    lists:nth(Idx, Data);
replace_var(Var, Data) when is_map(Var) ->
    replace_map_var(Var, Data);
replace_var(Var, _Data) -> Var.

uuid() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).
