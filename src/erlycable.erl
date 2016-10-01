%% Copyright
-module(erlycable).
-author(palkan).
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").
-define(APPS, [lager, deliverly]).

%% ------------------------------------------------------------------
%% Common Application Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/0, upgrade/0, ping/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-define(SERVER, erlycable_server).

start() ->
  ulitos_app:ensure_started(?APPS),
  application:start(erlycable).

stop() ->
  application:stop(erlycable).

upgrade() ->
 ulitos_app:reload(erlycable),
 ok.
 
ping() ->
  pong.
