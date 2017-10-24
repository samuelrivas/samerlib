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

%%% @doc useful functions to write sensible tests

-module(sel_test).

-export([test_in_dir/1, test_in_dir/2, props_to_eunit/1, props_to_eunit/2]).

%% See the documentation
-deprecated({test_in_dir, 2}).

-include_lib("eunit/include/eunit.hrl").

%%%_* Types ============================================================

%% Define the type dialyzer wants for Eunit tests. This should be really defined
%% by Eunit, but it isn't so far
-type eunit_test() :: {[_], {_, _, _}}.

%%%_* API ==============================================================

%% @doc Create clean directories to run tests that require actual files.
%%
%% This function creates an empty directory in /tmp, runs `Fun' with that
%% directory as argument, and deletes the directory and all its contents
%%
%% Returns the value returned by `Fun', letting any exception raised through
%% (but still deleting the directory)
-spec test_in_dir(fun((string()) -> any())) -> any().
test_in_dir(Fun) ->
    test_in_dir(true, Fun).

%% @doc Same as {@link test_in_dir/1}, but cleaning up only if `Cleanup' is
%% true.
%%
%% This function is intended only for debugging. Ideally, code using it should
%% never be committed to a public repository.
%%
%% @deprecated use it only for dirty local debugging, for committed code use
%% {@link test_in_dir/1}
-spec test_in_dir(boolean(), fun((string()) -> any())) -> any().
test_in_dir(Cleanup, Fun) ->
    Dir = create_rand_dir(),
    Res = try Fun(Dir)
          after delete_if(Cleanup, Dir)
          end,
    Res.

%% @equiv props_to_eunit(Module, 10)
-spec props_to_eunit(module()) -> [eunit_test()].
props_to_eunit(Module) -> props_to_eunit(Module, 10).

%% @doc Return an Eunit test that checks all properties in a module.
%%
%% To get all properties tested as part of the eunit suite (e.g. when running
%% `<module>:test()') you must include something like this in your test module:
%% ```
%% all_props_test_() -> sel_test:props_to_eunit(?MODULE).
%% '''
%% Currently, only proper properties are supported.
-spec props_to_eunit(module(), non_neg_integer()) -> [any()].
props_to_eunit(Module, Timeout) ->
    [{atom_to_list(P), eunitise(Module, P, Timeout)}
     || P <- module_properties(Module)].

%%%_* Internals ========================================================

delete_if(true, Dir) -> sel_file:delete_recursive(Dir);
delete_if(false, _) -> ok.

%% XXX Note we don't care much about seeding the psudorandom generator. This
%% must work with any pseudorandom sequence. Also, not seeding it eases testing,
%% as we can easily repeat any possible failing sequence (not likely, though)
create_rand_dir() ->
    create_rand_dir(max_dir_attemps()).

create_rand_dir(0) ->
    %% Something fishy is happening with the tests. Dirs should've been cleaned
    %% up and seems they are flooding the tmp dir.
    throw(cannot_generate_dir);

create_rand_dir(N) ->
    try
        FileName = random_dir_name(),
        Path = filename:join(base_test_dir(), FileName),
        sel_file:make_dir(Path),
        Path
    catch
        eexist -> create_rand_dir(N-1)
    end.

random_dir_name() ->
    "sel_test_temp-" ++ [random_letter() || _ <- lists:seq(1, 10)].

random_letter() ->
    rand:uniform($z - $a) + $a - 1.

base_test_dir() -> "/tmp".

max_dir_attemps() -> 10.

module_properties(Module) ->
    [F || {F, 0} <- Module:module_info(exports) , is_property(F)].

is_property(Fun) -> lists:prefix("prop_", atom_to_list(Fun)).

eunitise(Module, Property, Timeout) ->
    {timeout, Timeout, ?_assert(proper:quickcheck(Module:Property()))}.
