%%% -*- mode: erlang -*-
%%%
%%% I mostly erased OTP release handling applications from my toolset in favour
%%% of very simplistic scrtips. Thus this file might be outdated and in need of
%%% slight changes in order to integrate this application in an OTPish release

{application, crashy,
 [{description, "Functions to support aggressive let-it-crash style"},
  {vsn, "devel"},
  {modules, [crashfy, crash_file]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []}]}.
