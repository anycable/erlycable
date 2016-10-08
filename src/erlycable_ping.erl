-module(erlycable_ping).
-behaviour(gen_fsm).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, ping/2, handle_sync_event/4, handle_event/3, handle_info/3, terminate/3,
  code_change/4]).

-record(state, {
  timeout = 3000 ::non_neg_integer()
}).

%% API
start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Timeout = ?Config(ping_timeout, 3000),
  {ok, ping, #state{timeout = Timeout}, Timeout}.

ping(timeout, #state{timeout = T} = State) ->
  erlycable_server:broadcast(ping_message(ulitos:timestamp())),
  {next_state, ping, State, T}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, undefined, StateName, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
ping_message(Time) ->
  BinTime = integer_to_binary(Time),
  << <<"{\"type\":\"ping\",\"message\":">>/binary, BinTime/binary, <<"}">>/binary >>.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ping_message_test() ->
  ?assertEqual(<<"{\"type\":\"ping\",\"message\":123}">>, ping_message(123)).

-endif.
