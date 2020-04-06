-module(counter_trans).

-include_lib("syntax_tools/include/merl.hrl").

-export([parse_transform/2]).

%% ================================================
%% Trans counter names to list of index/1 functions
%% ================================================

parse_transform(AST, _Opts) ->
    %io:format("AST: ~p, Opts: ~p~n", [AST, _Opts]),
    ResAST = trans(AST, [], []),
    %io:format("ResAST: ~p~n", [ResAST]),
    ResAST.

trans([], Counters, ResAST) ->
    lists:reverse([f_counter_index(Counters) | ResAST]);
trans([{eof, L} | AST], Counters, ResAST) ->
    lists:reverse(
        [{eof, L},
         f_counter_index(Counters),
         f_counter_size(Counters),
         f_counters(Counters)
         | ResAST]) ++ AST;
trans([{attribute, _, counters, Counters} | AST], _, ResAST) ->
    is_atom_list(Counters) orelse error({bad_counter_list, Counters}),
    trans(AST, Counters, ResAST);
trans([F | AST], Counters, ResAST) ->
    trans(AST, Counters, [F | ResAST]).

f_counters(Counters) ->
    merl:quote(
        lists:flatten(
            io_lib:format("'@counters'() -> ~w.", [Counters]))).

f_counter_size(Counters) ->
    merl:quote(
        lists:flatten(
            io_lib:format("'@size'() -> ~w.", [length(Counters)]))).

f_counter_index(Counters) ->
    Length = length(Counters),
    {FunLiterals, _} =
        lists:foldl(fun
            (Cntr, {Literal, Index}) when Index =:= Length ->
                {Literal ++ io_lib:format("'@index'('~s') -> ~w.", [Cntr, Index]), Index};
            (Cntr, {Literal, Index}) ->
                {Literal ++ io_lib:format("'@index'('~s') -> ~w;\n", [Cntr, Index]), Index + 1}
            end, {"", 1}, Counters),
    merl:quote(lists:flatten(FunLiterals)).

is_atom_list(Counters) when is_list(Counters) ->
    lists:all(fun(C) when is_atom(C) -> true; (_) -> false end, Counters);
is_atom_list(_) -> false.
