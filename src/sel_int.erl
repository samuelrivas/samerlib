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

%%% @doc
-module(sel_int).

%%%_* Exports ==========================================================
-export([extended_euclid/2, int_div/2]).

%%%_* Includes =========================================================

%%%_* Types ============================================================

-type non_zero_integer() :: neg_integer() | pos_integer().

%%%_* API ==============================================================

%% @doc Returns `{X, Y}' such that `X*A + Y*B == gcd(A, B)'
%%
%% This function is used by {@link gcd/2}, and by {@link mod_inv/2} to
%% efficiently find the greatest common divisor and the modulus inverse of
%% integer numbers

%% This is the recursive implementation of the Extended Euclidean
%% Algorithm.
%%
%% Assume that A > B (Otherwise, the first recursive step just swaps them, so
%% this condition is true for the rest of the algorithm)
%%
%% If B is 0 then we have found the solution in {1, 0}
%%
%% Otherwise, Let Q, R such that A = B*Q + R
%% We can find X' and Y' such that X'*B + Y'*R = gcd(B, R)
%%
%% Note that a) R is smaller than A and b) gcd(A, B) == gcd(B, R)
%%
%% Thus X'*B + Y'*R = gcd(A, B)          =>
%%      X'*B + Y'*(A - B*Q) = gcd(A, B)  =>
%%      (X' - Q*Y')*B + Y'*A = gcd(A, B) =>
%%      X = Y' and Y = X' - Q * Y'
-spec extended_euclid(integer(), integer()) -> {integer(), integer()}.
extended_euclid(_, 0) ->
    {1, 0};
extended_euclid(A, B) ->
    {Q, R} = int_div(A, B),
    {X, Y} = extended_euclid(B, R),
    {Y, X - Q * Y}.

%% @doc Returns `{Q, R}' such that `A == Q*B + R'
-spec int_div(integer(), non_zero_integer()) -> {integer(), integer()}.
int_div(A, B) ->
    {A div B, A rem B}.

%%%_* Private Functions ================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
