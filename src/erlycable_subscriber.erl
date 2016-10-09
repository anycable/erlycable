-module(erlycable_subscriber).
-behaviour(gen_fsm).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").

%% API
-export([start_link/0]).

%% gen_server
-export([init/1, subscribing/2, subscribed/2, handle_sync_event/4, handle_event/3, handle_info/3, terminate/3,
  code_change/4]).

-record(state, {
  client ::pid()
}).

%% API
start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Host = ?Config(redis_host, "localhost"),
  Port = ?Config(redis_port, 6379),
  Channel = ?Config(redis_channel, <<"anycable">>),

  {ok, Client} = eredis_sub:start_link(Host, Port, ""),
  eredis_sub:controlling_process(Client, self()),
  eredis_sub:subscribe(Client, [Channel]),

  ?I({<<"Init Redis subscription">>, Host, Port, Channel}),

  {ok, subscribing, #state{client = Client}}.

subscribing(Msg, State) ->
  ?D({unknown_message, Msg}),
  {next_state, subscribing, State}.

subscribed(Msg, State) ->
  ?D({unknown_message, Msg}),
  {next_state, subscribed, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, undefined, StateName, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_info({subscribed, _, Sub}, subscribing, State) ->
  ?I(<<"Subscribed to Redis">>),
  eredis_sub:ack_message(Sub),
  {next_state, subscribed, State};

handle_info({message, _, Data, Sub}, subscribed, State) ->
  Msg = jsx:decode(Data, [return_maps]),
  case Msg of
    #{ <<"stream">> := Stream, <<"data">> := Message} ->
      ?I({broadcast, Stream, Message}),
      erlycable_server:broadcast(Stream, Message);
    Else -> ?D({unknown_message, Else})
  end,
  eredis_sub:ack_message(Sub),
  {next_state, subscribed, State};

handle_info(_Info, StateName, State) ->
  ?D({unknown_info, _Info}),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
