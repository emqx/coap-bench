-module(coap_bench_utils).

-export([replace_map_var/2]).

-export([bin_to_hexstr/1,hexstr_to_bin/1]).

replace_map_var(Map, Vars) ->
    maps:map(fun(_, V) ->
            replace_var(V, Vars)
        end, Map).

replace_var(<<"$payload_19_0_0">>, _Vars) ->
    <<2,0,1,0,2,65,66>>;
replace_var(<<"$payload_3_0">>, _Vars) ->
    <<200,0,20,79,112,101,110,32,77,111,98,105,108,101,32,65,108,108,105,97,110,99,101,200,1,22,76,105,103,104,116,119,101,105,103,104,116,32,77,50,77,32,67,108,105,101,110,116,200,2,9,51,52,53,48,48,48,49,50,51,195,3,49,46,48,134,6,65,0,1,65,1,5,136,7,8,66,0,14,216,66,1,19,136,135,8,65,0,125,66,1,3,132,193,9,19,193,10,15,131,11,65,0,0,200,13,8,0,0,0,0,176,14,92,138,198,14,43,48,49,58,48,48,200,15,13,69,117,114,111,112,101,47,66,101,114,108,105,110,193,16,85>>;
replace_var(<<"$uuid">>, _Vars) -> uuid();
replace_var(<<"$", Int/binary>> = Plhd, Vars) when is_list(Vars) ->
    Len = length(Vars),
    try binary_to_integer(Int) of
        Idx when Idx >= 1, Idx =< Len  ->
            lists:nth(Idx, Vars);
        Idx ->
            error({invalid_var_index, Idx, Vars})
    catch
        error:badarg ->
            error({unsupported_placeholder, Plhd})
    end;

replace_var(Var, Vars) when is_map(Var) ->
    replace_map_var(Var, Vars);
replace_var(Var, _Vars) -> Var.

uuid() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) when is_binary(S) ->
  hexstr_to_bin(binary_to_list(S), []);
hexstr_to_bin(S) when is_list(S) ->
  hexstr_to_bin(S, []).

hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).