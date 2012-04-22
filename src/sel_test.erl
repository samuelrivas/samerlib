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

-export([test_in_dir/1]).

%% @doc creates clean directories to run tests that require actual files
%%
%% This function creates an empty directory in /tmp, runs `Fun' with that
%% directory as argument, and deletes the directory and all its contents
%%
%% Returns the value returned by `Fun', letting any exception raised through
%% (but still deleting the directory)
-spec test_in_dir(fun((string()) -> any())) -> any().
test_in_dir(Fun) ->
    Dir = create_rand_dir(),
    Res = try Fun(Dir)
          after sel_file:delete_recursive(Dir)
          end,
    Res.

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
    random:uniform($z - $a) + $a - 1.

base_test_dir() -> "/tmp".

max_dir_attemps() -> 10.
