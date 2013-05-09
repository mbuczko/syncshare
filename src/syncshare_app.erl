%% @private
-module(syncshare_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->

    lager:set_loglevel(lager_console_backend, info),

    % init connection to RabbitMQ
    {ok, Connection, Channel} = syncshare_amqp:init(),

    % create service and RPC exchanges
    syncshare_amqp:declare_exchanges(Channel, [<<"twitter">>, <<"rembranto">>]),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/syncshare/sse/:service", sse_handler, [{channel, Channel}]},
			{"/syncshare/sse/:service/frame", frame_handler, [{channel, Channel}]},
			{"/syncshare/sse/:service/:call/:queue", xhr_handler, [{channel, Channel}]},

			{"/syncshare/wbs/:service", wbs_handler, [{channel, Channel}]},

            % TODO: remove in production.
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
