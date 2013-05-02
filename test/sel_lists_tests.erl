%%% Copyright (c) 2012, Samuel Rivas <samuelrivas@gmail.com>
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * Neither the name the author nor the names of its contributors may
%%%       be used to endorse or promote products derived from this software
%%%       without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%% @doc functions to test sel_lists module

-module(sel_lists_tests).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Eunit wrapper
%%%-------------------------------------------------------------------
all_props_test_() -> sel_test:props_to_eunit(?MODULE).

%%%-------------------------------------------------------------------
%%% EUnit tests
%%%-------------------------------------------------------------------
reduce_empty_list_test() ->
    ?assertError(function_clause, sel_lists:reduce(fun(_, _) -> foo end, [])).

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------

%% We get a valid result when searching for any element in the list
prop_keysearch_pos() ->
    ?FORALL(
       L, tuple_list(),
       begin
           Wrong =
               lists:filter(
                 fun(Tuple) ->
                         sel_lists:keysearch(key(Tuple), L) /= Tuple
                 end,
                 L),
           equals([], Wrong)
       end).

%% We get a valid exception result when searching for any element not in the
%% list
prop_keysearch_neg() ->
    ?FORALL(
       L, proper_types:non_empty(tuple_list()),
       ?FORALL(
          Tuple, external_tuple(L),
          try
              sel_lists:keysearch(key(Tuple), L),
              false
          catch
              {not_found, Key} ->
                  equals(Key, key(Tuple))
          end)).

prop_take_last() -> generic_take_property(take_last, suffix).

prop_take() -> generic_take_property(take, prefix).

prop_cut_and_zip() ->
    ?FORALL(
       {L1, L2}, {int_list(), int_list()},
       begin
           Zipped   = sel_lists:cut_and_zip(L1, L2),
           {X1, X2} = lists:unzip(Zipped),
           proper:conjunction(
             [{length  , equals(min(length(L1), length(L2)), length(Zipped))},
              {prefix_1, lists:prefix(X1, L1)},
              {prefix_2, lists:prefix(X2, L2)}
             ])
       end).

prop_reduce() ->
    ?FORALL(
       {X, N}, {proper_types:integer(), proper_types:pos_integer()},
       begin
           L = lists:seq(X, X + N - 1),
           proper:equals(progression_sum(X, N), sel_lists:reduce(sum(), L))
       end).

prop_drop() -> generic_drop_property(drop, suffix).

prop_drop_last() -> generic_drop_property(drop_last, prefix).

%%%-------------------------------------------------------------------
%%% Property Templates
%%%-------------------------------------------------------------------
generic_take_property(TakeFun, ListCheck) ->
    ?FORALL(
       {N, L}, {proper_types:non_neg_integer(), int_list()},
       begin
           Last           = sel_lists:TakeFun(N, L),
           ExpectedLength = min(N, length(L)),
           proper:conjunction(
             [{length, equals(length(Last), ExpectedLength)},
              {suffix, lists:ListCheck(Last, L)}])
       end).

generic_drop_property(DropFun, ListCheck) ->
    ?FORALL(
       {N, L}, {proper_types:non_neg_integer(), int_list()},
       begin
           Result         = sel_lists:DropFun(N, L),
           ExpectedLength = max(0, length(L) - N),
           proper:conjunction(
             [{length, equals(length(Result), ExpectedLength)},
              {ListCheck, lists:ListCheck(Result, L)}])
       end).

%%%-------------------------------------------------------------------
%%% Generators
%%%-------------------------------------------------------------------
int_list() -> proper_types:list(proper_types:integer()).

tuple_list() ->
    ?LET(L, proper_types:list(small_tuple()), filter_duplicated_keys(L)).

%% proper_types:tuple() generates huge tuples and has the risk of depleting the
%% atom table (probably due to a bug). The type of the tuple is not really
%% relevant, and for the size only a representation of some common sizes is
%% enough to have pretty strong tests.
small_tuple() ->
    Int = proper_types:integer(),
    proper_types:union([{Int}, {Int, Int}, {Int, Int, Int}]).


filter_duplicated_keys([]) ->
    [];
filter_duplicated_keys([H | T]) ->
    [H | filter_duplicated_keys(keydelete(key(H), T))].

%% lists:keydelete removes only the first instance
keydelete(Key, List) -> [Tuple || Tuple <- List, Key /= key(Tuple)].

key(Tuple) -> element(1, Tuple).

external_tuple(L) ->
    ?SUCHTHAT(T, small_tuple(), not lists:keymember(key(T), 1, L)).

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------
equals(A, B) -> proper:equals(A, B).

sum() -> fun(X, Y) -> X + Y end.

progression_sum(First, N) ->
    Last = First + N - 1,
    trunc(N * (First + Last) / 2).
