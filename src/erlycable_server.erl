-module(erlycable_server).
-author(palkan).
-behaviour(deliverly_handler).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Deverly Handler callbacks.
-export([authorize/2, handle_message/2, handle_client_message/2, client_disconnected/1]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  {ok, #state{}}.

authorize(Client, _) ->
  gen_server:cast(?SERVER, {connect, Client}),
  {ok, Client}.

handle_message(_,_) -> ok.

handle_client_message(Client, Raw) ->
  Message = json_encoder:decode(Client, Raw),
  gen_server:call(?SERVER, {handle_client_message, Message, Client, Raw}).

client_disconnected(#de_client{socket = _Socket}) -> 
  ok.

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast({connect, _Client}, State) ->
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
