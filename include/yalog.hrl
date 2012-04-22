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

-ifdef(NOLOGS).
-define(INF(Format), ok).
-define(INF(Format, Args), ok).
-define(WAR(Format), ok).
-define(WAR(Format, Args), ok).
-define(ERR(Format), ok).
-define(ERR(Format, Args), ok).
-define(DBG(Format), ok).
-define(DBG(Format, Args), ok).
-else.
-define(INF(Format), yalog:info(?MODULE, Format)).
-define(INF(Format, Args), yalog:info(?MODULE, Format, Args)).
-define(WAR(Format), yalog:warning(?MODULE, Format)).
-define(WAR(Format, Args), yalog:warning(?MODULE, Format, Args)).
-define(ERR(Format), yalog:error(?MODULE, Format)).
-define(ERR(Format, Args), yalog:error(?MODULE, Format, Args)).
-define(DBG(Format), yalog:debug(?MODULE, ?LINE, Format)).
-define(DBG(Format, Args), yalog:debug(?MODULE, ?LINE, Format, Args)).
-endif.
