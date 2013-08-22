%% @private
-module(syncshare_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->

    lager:set_loglevel(lager_console_backend, info),

    erlydtl:compile("templates/frame.dtl", "frame_dtl", [{out_dir, "ebin"}]),

    % initialize connection to RabbitMQ
    {ok, _Connection, Channel} = syncshare_amqp:init(),

    % create services exchanges
    syncshare_amqp:declare_exchanges(Channel, [<<"twitter">>, <<"rembranto">>]),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/syncshare/:service", frame_handler, [{channel, Channel}]},
			{"/syncshare/sse/:service", sse_handler, [{channel, Channel}]},
			{"/syncshare/sse/:service/:call", xhr_handler, [{channel, Channel}]},
			{"/syncshare/wbs/:service", wbs_handler, [{channel, Channel}]},

            {"/syncshare/providers/[...]", cowboy_static, [
                                                           {directory, {priv_dir, syncshare, []}},
                                                           {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                                                          ]}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),

	syncshare_sup:start_link().


stop(_State) ->
	ok.
