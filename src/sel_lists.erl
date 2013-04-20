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

%%% @doc Functions to handle lists
%%%
%%% Compared to the official `lists' module, this one aspires to have a
%%% cleaner output type definition, avoiding tagged values by using exceptions
%%% to handle errors

-module(sel_lists).

-export([keysearch/2, cut_and_zip/2, take_last/2]).

%% @doc Return the first tuple in the list with `Key' as first element
%%
%% @throws {not_found, any()}
-spec keysearch(any(), [tuple()]) -> any().
keysearch(Key, L) ->
    case lists:keysearch(Key, 1, L) of
        {value, Tuple} -> Tuple;
        false -> throw({not_found, Key})
    end.

%% @doc Like `lists:zip/2', but works with lists of different lengths
%%
%% When used with lists of different lengths, the last elements of the longest
%% list are discarded to make both lists of the same length
-spec cut_and_zip(List1, List2) -> List3 when
      List1 :: [A],
      List2 :: [B],
      List3 :: [{A, B}],
      A :: term(),
      B :: term().
cut_and_zip([], _L2) -> [];
cut_and_zip(_L1, []) -> [];
cut_and_zip([H1|T1], [H2|T2]) -> [{H1, H2} | cut_and_zip(T1, T2)].

%% @doc Return the last `N' elements of `L'
%%
%% Returns `L' if it has less than `N' elements
-spec take_last(N::non_neg_integer(), L::list(A)) ->list(A).
take_last(N, L) when length(L) =< N -> L;
take_last(N, [_|T]) -> take_last(N, T).
