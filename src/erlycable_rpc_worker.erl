-module(erlycable_rpc_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  client,
  port ::inet:port_number()|undefined,
  host ::inet:ip_address()|inet:ip_hostname()|undefined
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(#{ host := Host, port := Port}) ->
  {ok, Client} = erlgrpc:dial(#{ host => Host, port => Port }),
  {
    ok, 
    #state{
      client = Client,
      host = Host,
      port = Port
    }
  }.

handle_call({invoke, Method, Data}, _From, #state{client=Client} = State) ->
  Res = erlgrpc:invoke(Client, Method, Data),
  case Res of
    %% restart process if no connection
    {error, no_connection} -> {stop, no_connection, State};
    _ -> {reply, Res, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{client=Client}) ->
  erlgrpc:close(Client),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
