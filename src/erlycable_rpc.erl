-module(erlycable_rpc).
-include_lib("deliverly/include/deliverly.hrl").
-include_lib("erlycable/include/erlycable.hrl").
-include_lib("erlycable/include/log.hrl").
-include_lib("erlycable/include/priv.hrl").
-include_lib("erlycable/include/anycable_pb.hrl").
-export([connect/2, disconnect/2, subscribe/3, unsubscribe/2, perform/3]).

%% @doc
%% Make 'Connect' call to RCP server to authorize connection.
%% Updates client with connection identifiers on success.
%% @end
-spec connect(Path::binary(), Headers::list()) -> {ok, #'ConnectionResponse'{}} | {error, any()}.
connect(Path, Headers) ->
  Msg = anycable_pb:encode_msg(#'ConnectionRequest'{path = Path, headers=Headers}),
  Res = poolboy:transaction(?RPC_POOL,
    fun(Worker) ->
      gen_server:call(Worker, {invoke, <<"/anycable.RPC/Connect">>, Msg})
    end
  ),
  case Res of
    {error, Reason} ->
      ?E({rpc_error, Reason}),
      {error, Reason};
    {ok, Data} ->
      {ok, anycable_pb:decode_msg(Data, 'ConnectionResponse')}
  end.

%% @doc
%% Make 'Disconnect' call to RCP server after connection has been closed.
%% @end
-spec disconnect(Identifiers::binary(), Subscriptions::list()) -> {ok, #'DisconnectResponse'{}} | {error, any()}.
disconnect(Identifiers, Subscriptions) ->
  Msg = anycable_pb:encode_msg(#'DisconnectRequest'{identifiers = Identifiers, subscriptions = Subscriptions}),
  Res = poolboy:transaction(?RPC_POOL,
    fun(Worker) ->
      gen_server:call(Worker, {invoke, <<"/anycable.RPC/Disconnect">>, Msg})
    end
  ),
  case Res of
    {error, Reason} ->
      ?E({rpc_error, Reason}),
      {error, Reason};
    {ok, Data} ->
      {ok, anycable_pb:decode_msg(Data, 'DisconnectResponse')}
  end.

%% @doc
%% Make 'Command' call to RCP server with "subscribe" command.
%% Register client subscription on success.
%% @end
-spec subscribe(Identifiers::binary(), Channel::binary(), Params::binary()) -> {ok, #'CommandResponse'{}} | {error, any()}.
subscribe(Identifiers, Channel, _Params) ->
  Msg = anycable_pb:encode_msg(#'CommandMessage'{
    command = <<"subscribe">>,
    identifier = Channel,
    connection_identifiers = Identifiers
  }),
  invoke_command(<<"/anycable.RPC/Command">>, Msg).

%% @doc
%% Make 'Command' call to RCP server with "unsubsribe" command.
%% Unregister client subscription on success.
%% @end
-spec unsubscribe(Identifiers::binary(), Channel::binary()) -> {ok, #'CommandResponse'{}} | {error, any()}.
unsubscribe(Identifiers, Channel) ->
  Msg = anycable_pb:encode_msg(#'CommandMessage'{
    command = <<"unsubscribe">>,
    identifier = Channel,
    connection_identifiers = Identifiers
  }),
  invoke_command(<<"/anycable.RPC/Command">>, Msg).

%% @doc
%% Make 'Command' call to RCP server with "message" command.
%% @end
-spec perform(Identifiers::binary(), Channel::binary(), Data::binary()) -> {ok, #'CommandResponse'{}} | {error, any()}.
perform(Identifiers, Channel, Data) ->
  Msg = anycable_pb:encode_msg(#'CommandMessage'{
    command = <<"message">>,
    identifier = Channel,
    connection_identifiers = Identifiers,
    data = Data
  }),
  invoke_command(<<"/anycable.RPC/Command">>, Msg).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
invoke_command(Command, Msg) ->
  Res = poolboy:transaction(?RPC_POOL,
    fun(Worker) ->
      gen_server:call(Worker, {invoke, Command, Msg})
    end
  ),
  case Res of
    {error, Reason} ->
      ?E({rpc_error, Reason}),
      {error, Reason};
    {ok, Data} ->
      {ok, anycable_pb:decode_msg(Data, 'CommandResponse')}
  end.
