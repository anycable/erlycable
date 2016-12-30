-module(erlycable_disconnector).
-behaviour(gen_server).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").
-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, disconnect/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
  {ok, {}}.

disconnect(Identifiers, Subscriptions) ->
  gen_server:cast(?SERVER, {disconnect, Identifiers, Subscriptions}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({disconnect, Identifiers, Subscriptions}, State) ->
  erlycable_rpc:disconnect(Identifiers, Subscriptions),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
