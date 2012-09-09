%%% -*- mode: erlang -*-
%%%
%%% I mostly erased OTP release handling applications from my toolset in favour
%%% of very simplistic scrtips. Thus this file might be outdated and in need of
%%% slight changes in order to integrate this application in an OTPish release

{application, samerlib,
 [{description, "A bucket for low level, very common functionality"},
  {vsn, "devel"},
  {modules, [crashfy, crash_file, sel_application, sel_file, sel_gen_server,
             sel_int, sel_lists, sel_process, sel_string, sel_test, yalog]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []}]}.
