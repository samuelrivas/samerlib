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

%%% @doc Yet another logger
%%%
%%% @end

-module(yalog).

-include_lib("samerlib/include/yalog.hrl").

-export([info/2, info/3, warning/2, warning/3, error/2, error/3, debug/3,
         debug/4]).

-export([test/0]).

test() ->
    ?INF("Test"),
    ?INF("Test ~s", ["with arguments"]),
    ?WAR("Test"),
    ?WAR("Test ~s", ["with arguments"]),
    ?ERR("Test"),
    ?ERR("Test ~s", ["with arguments"]),
    ?DBG("Test"),
    ?DBG("Test ~s", ["with arguments"]).

info(Module, Format) -> info(Module, Format, []).

info(Module, Format, Args) ->
    log(info, Module, Format, Args).

warning(Module, Format) -> warning(Module, Format, []).

warning(Module, Format, Args) ->
    log(warning, Module, Format, Args).

error(Module, Format) -> error(Module, Format, []).

error(Module, Format, Args) ->
    log(error, Module, Format, Args).

debug(Module, Line, Format) -> debug(Module, Line, Format, []).

debug(Module, Line, Format, Args) ->
    log(debug, Module, Line, Format, Args).

log(Type, Module, Format, Args) ->
    log(Type, Module, none, Format, Args).

log(Type, Module, Line, Format, Args) ->
    io:format(format_entry(Type, just_now(), Module, Line, Format, Args)).

just_now() -> calendar:now_to_universal_time(now()).

format_entry(Type, Time, Module, Line, Format, Args)
  when is_list(Args) ->
    [format_type(Type), " [", format_date(Time), " ",
     format_source(Module, Line), "] ", io_lib:format(Format, Args), "\r\n"].

format_type(info) -> "INF";
format_type(warning) -> "WAR";
format_type(error) -> "ERR";
format_type(debug) -> "DBG".

format_date({{Y, Mo, D}, {H, Mi, S}}) ->
    io_lib:format(
      "~4..0b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b", [Y, Mo, D, H, Mi, S]).

format_source(Module, Line) -> [atom_to_list(Module) | format_line(Line)].

format_line(none) -> [];
format_line(Line) -> io_lib:format(":~p", [Line]).
