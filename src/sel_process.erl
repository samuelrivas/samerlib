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

%%% @doc Generic functions to handle processes

-module(sel_process).

-export([wait_exit/1, wait_exit/2, get_name/1, get_pid/1]).

%% @doc Wait until `Pid' exits and return its exit reason
%%
%% Returns `noproc' if the process has already finished before evaluating this
%% function
-spec wait_exit(pid()) -> noproc | any().
wait_exit(Pid) when is_pid(Pid) ->
    wait_exit(Pid, default_timeout()).

%% @doc Like {@link wait_exit/1} but with a timeout
%%
%% This function throws `timeout' if the process doesn't finish before `Timeout'
%% milliseconds.
%%
%% @throws timeout
-spec wait_exit(pid(), pos_integer() | infinity) -> noproc | any().
wait_exit(Pid, Tiemout) ->
    Ref = erlang:monitor(process, Pid),

    receive {'DOWN', Ref, _Type, Pid, Reason} -> Reason
    after Tiemout -> throw(timeout)
    end.

%% @doc Return the registered name of `Pid'
%%
%% This function fails if the process is not registered or doesn't exist
%% @throws {not_registered, pid()} | {nonexistent_process, pid()}
-spec get_name(pid()) -> atom().
get_name(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        []                      -> throw({not_registered, Pid});
        undefined               -> throw({nonexistent_process, Pid})
    end.

%% @doc Return the pid associated to a registered name
%%
%% This function fails if no pid is registered with the provided name
%% @throws {invalid_process_name, atom()}
-spec get_pid(atom()) -> pid().
get_pid(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> throw({invalid_process_name, Name});
        Pid       -> Pid
    end.

%% Milliseconds
default_timeout() -> infinity.
