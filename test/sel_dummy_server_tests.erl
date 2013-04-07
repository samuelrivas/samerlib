%%% Copyright (c) 2013, Samuel Rivas <samuelrivas@gmail.com>
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

%%% @doc Unit tests for sel_dummy_server
-module(sel_dummy_server_tests).

-include_lib("eunit/include/eunit.hrl").

start_stop_anonymous_test_() ->
  test_template(
    fun() -> sel_dummy_server:start_link() end,
    fun(Pid) ->
        [?_assertEqual(ok, sel_gen_server:call(Pid, foo)),
         ?_assertEqual(ok, sel_dummy_server:stop(Pid))]
    end).

start_stop_named_test_() ->
  test_template(
   fun() -> sel_dummy_server:start_link(?MODULE) end,
   fun(_Pid) ->
        [?_assertEqual(ok, sel_gen_server:call(?MODULE, foo)),
         ?_assertEqual(ok, sel_dummy_server:stop(?MODULE))]
   end).

test_template(StartServerFun, TestsFun) ->
  {setup,
   fun() ->
       {ok, Pid} = StartServerFun(),
       Pid
   end,
   fun(_Pid) -> ok end,
   fun(Pid) ->
       {inorder,
        TestsFun(Pid)
        ++ [?_test(sel_process:wait_exit(Pid))]}
   end}.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
