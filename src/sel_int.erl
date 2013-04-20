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

%%% @doc Integer arithmetic operations

-module(sel_int).

%%%_* Exports ==========================================================
-export([extended_euclid/2, int_div/2, gcd/2, mod_abs/2, mod_inv/2, sqrt/1]).

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

%% @doc Returns the greatest common divisor of `A' and `B'
-spec gcd(integer(), integer()) -> pos_integer().
gcd(A, B) ->
    {X, Y} = extended_euclid(A, B),
    (A*X + B*Y).

%% @doc Returns the modulus absolute value
%%
%% That is, the equivalent of `A' modulus `Mod' in the range [0, `Mod' - 1]
-spec mod_abs(integer(), pos_integer()) -> non_neg_integer().
mod_abs(A, Mod) ->
    case A rem Mod of
        X when X < 0 -> Mod + X;
        X -> X
    end.

%% @doc Returns the inverse of `N' modulus `Mod'
%% @throws {no_inverse, {N, mod, Mod}}
-spec mod_inv(integer(), pos_integer()) -> pos_integer().
mod_inv(N, Mod) ->
    {A, _} = extended_euclid(mod_abs(N, Mod), Mod),
    Tentative = mod_abs(A, Mod),
    case mod_abs(Tentative * N, Mod) of
        1 -> Tentative;
        _ -> throw({no_inverse, {N, mod, Mod}})
    end.

%% @doc Returns the closest integers to the square root of `N'
%%
%% `Low' is the highest number such that `Low * Low =< N'
%%
%% `High' is the lowest number such that `High * High >= N'
%%
%% Note that if `Low' is equal to `High' then they are the exact square root
%% of `N'
%% @end
%% IMPLEMENTATION Current algorithm is just a plain binary search from 0 to
%% N. It is reasonably fast to compute square roots of very big numbers (for
%% numbers of 512 to 600 bits computing times are at millisecond level in an
%% intel core i5), but there is room for improvement if this function needs to
%% be used intensively. The module sel_int_bench, living in the test directory
%% must help when optimising this code
-spec sqrt(pos_integer()) -> {Low::pos_integer(), High::pos_integer()}.
sqrt(N) when N >= 0 -> sqrt_binary(N, 0, N).

sqrt_binary(_N, Low, High) when Low > High ->
    %% We failed to converge to an exact solution, so the non-integer square
    %% root must be between the swapped High and Low values
    {High, Low};
sqrt_binary(N, Low, High) ->
    %% Don't use / to find the midpoint here, for close bignums it can yield
    %% values higher than High
    Attempt = (Low + High) bsr 1,
    case Attempt * Attempt of
        N -> {Attempt, Attempt};
        Greater when Greater > N -> sqrt_binary(N, Low, Attempt - 1);
        Lower when Lower < N -> sqrt_binary(N, Attempt + 1, High)
    end.

%%%_* Private Functions ================================================

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
