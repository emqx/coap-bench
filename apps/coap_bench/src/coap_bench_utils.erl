-module(coap_bench_utils).

-export([ replace_map_var/2
        , interval/1
        ]).

replace_map_var(Map, Vars) ->
    maps:map(fun(K, V) ->
            replace_var(K, V, Vars)
        end, Map).

replace_var(_, <<"$payload_19_0_0">>, _Vars) ->
    <<2,0,1,0,2,65,66>>;
replace_var(_, <<"$payload_3_0">>, _Vars) ->
    <<200,0,20,79,112,101,110,32,77,111,98,105,108,101,32,65,108,108,105,97,110,99,101,200,1,22,76,105,103,104,116,119,101,105,103,104,116,32,77,50,77,32,67,108,105,101,110,116,200,2,9,51,52,53,48,48,48,49,50,51,195,3,49,46,48,134,6,65,0,1,65,1,5,136,7,8,66,0,14,216,66,1,19,136,135,8,65,0,125,66,1,3,132,193,9,19,193,10,15,131,11,65,0,0,200,13,8,0,0,0,0,176,14,92,138,198,14,43,48,49,58,48,48,200,15,13,69,117,114,111,112,101,47,66,101,114,108,105,110,193,16,85>>;
replace_var(_, <<"$uuid">>, _Vars) -> uuid();
replace_var(Key, <<"$", Int/binary>> = Plhd, Vars) when is_list(Vars) ->
    Len = length(Vars),
    try binary_to_integer(Int) of
        Idx when Idx >= 1, Idx =< Len  ->
            format_field_var(Key, lists:nth(Idx, Vars));
        Idx ->
            error({invalid_var_index, Idx, Vars})
    catch
        error:badarg ->
            error({unsupported_placeholder, Plhd})
    end;

replace_var(_, Var, Vars) when is_map(Var) ->
    replace_map_var(Var, Vars);
replace_var(_, Var, _Vars) -> Var.

format_field_var(Key, Var)
    when Key == <<"interval">>
       ; Key == <<"timeout">>
    -> interval(Var);
format_field_var(_, Var) -> Var.

uuid() ->
    uuid:uuid_to_string(uuid:get_v4(), binary_standard).

interval(infinity) -> infinity;
interval(Sec) when is_integer(Sec) -> timer:seconds(Sec);
interval(Interval) when is_binary(Interval) ->
    case re:run(Interval,"(\\d+)(ms|s|m|h|d)?",[{capture,all_but_first,list}]) of
        nomatch -> error({invalid_interval, Interval});
        {match,[Num]} ->
            timer:seconds(list_to_integer(Num));
        {match,[Num,"ms"]} ->
            list_to_integer(Num);
        {match,[Num,"s"]} ->
            timer:seconds(list_to_integer(Num));
        {match,[Num,"m"]} ->
            timer:seconds(list_to_integer(Num)) * 60;
        {match,[Num,"h"]} ->
            timer:seconds(list_to_integer(Num)) * 60 * 60;
        {match,[Num,"d"]} ->
            timer:seconds(list_to_integer(Num)) * 60 * 60 * 24
    end.