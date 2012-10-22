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

	Dispatch = [
		{'_', [
			{[<<"syncshare">>, <<"sse">>, '...'], sse_handler, [{connection, Connection},
                                                                {channel, Channel}]},
			{[<<"syncshare">>, <<"init">>], init_handler, []},
            {[<<"syncshare">>, <<"static">>, '...'], cowboy_static, [{directory, <<"./static">>}, 
                                                                     {mimetypes, [
                                                                                  {<<".css">>, [<<"text/css">>]}, 
                                                                                  {<<".html">>, [<<"text/html">>]},
                                                                                  {<<".js">>, [<<"application/javascript">>]}
                                                                                 ]}]}
		]}
	],
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{dispatch, Dispatch}
	]),
	syncshare_sup:start_link().

stop(_State) ->
	ok.
