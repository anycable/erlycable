-module(erlycable_sup).
-author(palkan).
-include_lib("erlycable/include/priv.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  SizeArgs = [
    {size, ?Config(pool_size, 50)},
    {max_overflow, ?Config(max_overflow, 5)}
  ],

  PoolArgs = [
    {name, {local, ?RPC_POOL}},
    {worker_module, erlycable_rpc_worker}
  ] ++ SizeArgs,

  WorkerArgs = #{ host => ?Config(rpc_host, "localhost"), port => ?Config(rpc_port, 50051)},

  PoolSpecs = [poolboy:child_spec(?RPC_POOL, PoolArgs, WorkerArgs)],

  Children = [
    ?CHILD(erlycable_server, worker),
    ?CHILD(erlycable_ping, worker),
    ?CHILD(erlycable_subscriber, worker)
  ],
  {ok, {{one_for_one, 5, 10}, Children ++ PoolSpecs}}.
