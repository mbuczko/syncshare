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

    % create exchanges: fanout and direct one for each service
    syncshare_amqp:init_exchanges(Channel, [<<"twitter">>, <<"rembranto">>]),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/init", init_handler, []},
			{"/sse/[...]", sse_handler, [{connection, Connection}, {channel, Channel}]},
            {"/[...]", cowboy_static, [{directory, {priv_dir, syncshare, []}},
									   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	syncshare_sup:start_link().

stop(_State) ->
	ok.
