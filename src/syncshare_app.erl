%% @private
-module(syncshare_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->

    % init connection to RabbitMQ
    {ok, Connection, Channel} = syncshare_amqp:init(),

    % create service and RPC exchanges
    syncshare_amqp:declare_exchanges(Channel, [<<"twitter">>, <<"rembranto">>]),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/syncshare/:service", sse_handler, [{channel, Channel}]},
			{"/syncshare/:service/init", ini_handler, [{channel, Channel}]},
			{"/syncshare/:service/direct/:message", message_handler, [{channel, Channel}]},

            % TODO: remove in production.
            {"/[...]", cowboy_static, [{directory, {priv_dir, syncshare, []}},
												 {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
%			{"/init", init_handler, []},
%			{"/sse/[...]", sse_handler, [{connection, Connection}, {channel, Channel}]},
%            {"/[...]", cowboy_static, [{directory, {priv_dir, syncshare, []}},
%									   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	syncshare_sup:start_link().

stop(_State) ->
	ok.
