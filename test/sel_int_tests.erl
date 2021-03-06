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

%%% @doc Proper tests for sel_int
-module(sel_int_tests).

%%%_* Includes =========================================================

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Eunit property wrapper ===========================================

all_props_test_() -> sel_test:props_to_eunit(?MODULE).

%%%_* Properties =======================================================

prop_extended_euclid() ->
    ?FORALL(
       {A, B}, pair(non_zero_int()),
       begin
           {X, Y} = sel_int:extended_euclid(A, B),
           D = X*A + Y*B,
           proper:conjunction(
             [{integer_x, is_integer(X)},
              {integer_y, is_integer(Y)}
              | gcd_conditions(A, B, D)])
       end).

prop_integer_division() ->
    ?FORALL(
       {A, B}, {proper_types:integer(), non_zero_int()},
       begin
           {Q, R} = sel_int:int_div(A, B),
           proper:conjunction(
             [{result    , proper:equals(A, Q * B + R)},
              {integer_q , is_integer(Q)},
              {integer_r , is_integer(R)},
              {sign_q    , Q*A*B >= 0},
              {sign_r    , R*A >= 0}])
       end).

prop_gcd() ->
    ?FORALL(
       {A, B}, pair(non_zero_int()),
       begin
           Gcd = sel_int:gcd(A, B),
           proper:conjunction(gcd_conditions(A, B, Gcd))
       end).

prop_mod_abs() ->
    ?FORALL(
       {A, Mod}, {proper_types:integer(), proper_types:pos_integer()},
       begin
           AmodB = sel_int:mod_abs(A, Mod),
           proper:conjunction(
             [{correct, proper:equals(0, (A - AmodB) rem Mod)}
              | mod_number_conditions(AmodB, Mod)])
       end).

prop_mod_inv() ->
    ?FORALL(
       {A, Mod}, coprime_pair(),
       begin
           Ainv = sel_int:mod_inv(A, Mod),
           proper:conjunction(
             [{inverse , proper:equals(1, sel_int:mod_abs(Ainv * A, Mod))},
              {not_zero, Ainv /= 0}
              | mod_number_conditions(Ainv, Mod)])
       end).

prop_no_mod_inv() ->
    ?FORALL(
       {A, Mod}, {proper_types:integer(), positive_integer()},
       try sel_int:mod_inv(A, Mod) of
           _ -> true
       catch
           {no_inverse, {A, mod, Mod}} ->
               sel_int:gcd(A, Mod) /= 1 orelse sel_int:mod_abs(A, Mod) =:= 0
       end).

prop_sqrt() ->
    ?FORALL(
       N, positive_integer(),
       begin
           {Min, Max} = sel_int:sqrt(N),
           proper:conjunction(
             [{diff         , Max - Min =< 1},
              {positive_max , Max >= 0},
              {positive_min , Min >= 0},
              {max          , Max * Max >= N},
              {min          , Min * Min =< N}])
       end).

%%%_* Generators =======================================================

non_zero_int()     -> ?SUCHTHAT(N, proper_types:integer(), N /= 0).
positive_integer() -> proper_types:pos_integer().
prime()            -> proper_types:oneof(primes()).
factors()          -> proper_types:non_empty(proper_types:list(prime())).

other_factors(Factors) ->
    Factor = proper_types:oneof(primes() -- Factors),
    proper_types:non_empty(proper_types:list(Factor)).

pair(Gen) -> {Gen, Gen}.

coprime_pair() ->
    ?LET(
       {Sign, FactorsA}, {proper_types:oneof([-1, 1]), factors()},
       ?LET(
          FactorsB, other_factors(FactorsA),
          {Sign * mult(FactorsA), mult(FactorsB)})).

mult(Factors) -> sel_lists:reduce(fun(A, B) -> A * B end, Factors).

%%%_* Private Functions ================================================

gcd_conditions(A, B, Gcd) ->
      [{integer            , is_integer(Gcd)},
       {divides_a          , proper:equals(0, A rem Gcd)},
       {divides_b          , proper:equals(0, B rem Gcd)},
       {no_greater_divisor , no_greater_divisor(A, B, Gcd)}].

no_greater_divisor(A, B, Gcd) ->
    Candidates = lists:seq(abs(Gcd) + 1, erlang:min(abs(A), abs(B))),
    not lists:any(
          fun(X) -> (A rem X) =:= 0 andalso (B rem X) =:= 0 end,
          Candidates).

mod_number_conditions(N, Mod) ->
    [{positive, N >= 0},
     {in_range, N < Mod}].

primes() ->
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
     73, 79, 83, 89, 97, 101].

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
