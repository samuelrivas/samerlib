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

%%% @doc convenience functions to handle files

-module(sel_file).

-export([delete_recursive/1, make_dir/1, write_file/2]).

%% XXX This function is a temporary hack, will be reimplemented in a sane(r) way
%% soon (story 28185287 in learnerl project)
%% @doc removes a directory and all its contents
-spec delete_recursive(string()) -> ok.
delete_recursive(Dir) ->
    os:cmd("rm -rf " ++ Dir),
    ok.

%% @doc a crashfied version of file:make_dir/1
%%
%% @see crashfy:untuple/1
-spec make_dir(string()) -> ok.
make_dir(Dir) -> crashfy:untuple(file:make_dir(Dir)).

%% @doc a crashfied version of file:write_file/1
%%
%% @see crashfy:untuple/1
-spec write_file(string(), iolist()) -> ok.
write_file(File, Data) -> crashfy:untuple(file:write_file(File, Data)).
