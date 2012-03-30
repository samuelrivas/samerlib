%%% -*- mode: erlang -*-
%%%
%%% I mostly erased OTP release handling applications from my toolset in favour
%%% of very simplistic scrtips. Thus this file might be outdated and in need of
%%% slight changes in order to integrate this application in an OTPish release

{application, samerlib,
 [{description, "Set of applications supporting my own coding style"},
  {vsn, "devel"},
  {modules, [crashfy, crash_file, yalog]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []}]}.
