-module(erlycable_app).
-author(palkan).
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ?I("Starting application: erlycable"),
  deliverly:register_handler(cable, erlycable_server),
  erlycable_sup:start_link().

stop(_State) ->
  ok.
