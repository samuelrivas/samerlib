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

%%% @doc functions to handle strings and formatting common types

-module(sel_string).

-export([parse_hex/1, format_byte/1, format_byte/2, format_hex/1,
         format_hex/2]).

-type letter_case() :: lower | upper.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Read a binary from its hexadecimal representation
%%
%% For example:
%% ```
%% parse_hex("00FF") -> <<0,255>>
%% '''
%% Hexadecimal characters can be either in upper case or lower case
%%
%% @throws invalid_hex_string
-spec parse_hex(string()) -> binary().
parse_hex(String) ->
    parse_hex(String, <<>>).

parse_hex("", Acc) ->
    Acc;
parse_hex([H1, H2 | T], Acc) ->
    Byte = list_to_integer([H1, H2], 16),
    parse_hex(T, <<Acc/binary, Byte>>);
parse_hex(_, _) ->
    throw(invalid_hex_string).

%% @equiv format_byte(Byte, upper)
-spec format_byte(byte()) -> string().
format_byte(Byte) -> format_byte(Byte, upper).

%% @doc Create a string with the hexadecimal representation of a byte
%%
%% Characters from a to f can be either in upper case or lower case depending on
%% the value of `LetterCase'
-spec format_byte(byte(), letter_case()) -> string().
format_byte(Byte, LetterCase) when 0 =< Byte, 255 >= Byte ->
    Format = get_hex_format(LetterCase),
    lists:flatten(io_lib:format(Format, [Byte])).

%% @equiv format_hex(B, upper)
-spec format_hex(binary()) -> iolist().
format_hex(B) -> format_hex(B, upper).

%% @doc Output the hexadecimal formatting of a binary
%%
%% For example:
%% ```
%% > io:format("~s~n", [sel_string:format_hex(<<"Hello world">>)]).
%%   48656C6C6F20776F726C64
%%'''
-spec format_hex(binary(), letter_case()) -> iolist().
format_hex(B, Case) ->
    [format_byte(Byte, Case) || Byte <- binary_to_list(B)].

%%%-------------------------------------------------------------------
%%% Internals
%%%-------------------------------------------------------------------

get_hex_format(lower) -> "~2.16.0b";
get_hex_format(upper) -> "~2.16.0B".
