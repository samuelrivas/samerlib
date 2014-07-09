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

%%% @doc This module tests {@link sel_process}

-module(sel_process_tests).

-define(PROPER_NO_IMPORTS, true).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Eunit tests
%%%-------------------------------------------------------------------
all_props_test_() -> sel_test:props_to_eunit(?MODULE).

bad_arguments_test_() ->
    [?_assertError(_, sel_process:wait_exit(foo)),
     ?_assertError(_, sel_process:wait_exit(10)),
     ?_assertError(_, sel_process:wait_exit("foo")),
     ?_assertError(_, sel_process:wait_exit({"foo", self()}))].

get_name_positive_test_() ->
    named_process_template(
      fun(Pid, TestName) ->
              ?_assertEqual(TestName, sel_process:get_name(Pid))
      end).

get_name_no_name_test_() ->
    anonymous_process_template(
      fun(Pid) ->
              ?_assertThrow({not_registered, Pid}, sel_process:get_name(Pid))
      end).

get_name_negative_test_() ->
    dead_process_template(
      fun(Pid) ->
              ?_assertThrow(
                 {nonexistent_process, Pid}, sel_process:get_name(Pid))
      end).

%%%-------------------------------------------------------------------
%%% Properties
%%%-------------------------------------------------------------------
prop_wait_exit() ->
    ?FORALL(
       {Millisecs, Reason}, {millisecs(), reason()},
       ?TRAPEXIT(
          begin
              Pid = spawn(fun() -> worker(Millisecs, Reason) end),
              try
                  ExitReason = sel_process:wait_exit(Pid),
                  proper:equals(Reason, ExitReason)
              after
                  exit(Pid, kill)
              end
          end)).

prop_timeout() ->
    ?FORALL(
       Millisecs, millisecs(),
       ?TRAPEXIT(
          begin
              Pid = spawn(fun() -> worker(Millisecs, ok) end),
              try
                  sel_process:wait_exit(Pid, Millisecs - 10),
                  false
              catch
                  timeout -> true
              after
                  exit(Pid, kill)
              end
          end)).

prop_no_proc() ->
    ?FORALL(
       Millisecs, millisecs(),
       ?TRAPEXIT(
          begin
              Pid = spawn(fun() -> worker(Millisecs, ok) end),
              try
                  sel_process:wait_exit(Pid),
                  Reason = sel_process:wait_exit(Pid),
                  proper:equals(Reason, noproc)
              after
                  exit(Pid, kill)
              end
          end)).

%%%-------------------------------------------------------------------
%%% Eunit test templates
%%%-------------------------------------------------------------------

named_process_template(TestFun) ->
    TestName = sel_process_tests_test_process,
    {setup,
     fun() ->
             spawn(fun() -> named_process(TestName) end)
     end,
     fun(Pid) ->
             exit(Pid, kill),
             sel_process:wait_exit(Pid)
     end,
     fun(Pid) -> TestFun(Pid, TestName) end}.

anonymous_process_template(TestFun) ->
    {setup,
     fun() ->
             spawn(fun() -> anonymous_process() end)
     end,
     fun(Pid) -> exit(Pid, kill) end,
     fun(Pid) -> TestFun(Pid) end}.

dead_process_template(TestFun) ->
    {setup,
     fun() ->
             Pid = spawn(fun() -> anonymous_process() end),
             exit(Pid, kill),
             sel_process:wait_exit(Pid),
             Pid
     end,
     fun(Pid) -> TestFun(Pid) end}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% Wait at least 10 msecs to avoid monitoring a process that already died
millisecs() -> proper_types:range(10, 100).

reason() -> {reason, proper_types:int()}.

worker(Millisecs, Reason) ->
    timer:sleep(Millisecs),
    exit(Reason).

named_process(Name) ->
    register(Name, self()),
    timer:sleep(infinity).

anonymous_process() ->
    timer:sleep(infinity).
