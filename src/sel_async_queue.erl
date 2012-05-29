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

%%% @doc Simple asynchronous FIFO queue
%%%
%%% Pushing is non-blocking, but popping will block if the queue is empty.

-module(sel_async_queue).

-behaviour(gen_server).

%% API
-export([new/0, push/2, pop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-opaque async_queue() :: pid().

-export_type([async_queue/0]).

-record(state, {queue :: queue(),
                susps :: queue()}). % Suspended calls to pop are here

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc returns an empty queue
-spec new() -> async_queue().
new() ->
    crashfy:untuple(gen_server:start_link(?MODULE, none, [])).

%% @doc adds a new element to a queue
-spec push(async_queue(), any()) -> ok.
push(Q, Item) -> sel_gen_server:call(Q, {push, Item}).

%% @doc gets the oldest element from the queue
%%
%% If the queue is empty this function blocks if the queue is empty. In that
%% case it will return the next element pushed into it.
-spec pop(async_queue()) -> any().
pop(Q) -> sel_gen_server:call(Q, pop, infinity).

%%%-------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------

%% @private
init(none) ->
    {ok, #state{queue = queue:new(), susps = queue:new()}}.

%% @private
handle_call({push, Item}, _From, State = #state{queue = Queue}) ->
    {reply, ok, State#state{queue = queue:in(Item, Queue)}};
handle_call(pop, From, State = #state{queue = Queue, susps = Susps}) ->
    case queue:is_empty(Queue) of
        true ->
            {noreply, State#state{susps = queue:in(From, Susps)}};
        false ->
            {{value, Item}, NewQueue} = queue:out(Queue),
            {reply, Item, State#state{queue = NewQueue}}
    end;
handle_call(Request, _From, State) ->
    {reply, {error, {bad_call, Request}}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
