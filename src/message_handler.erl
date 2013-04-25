%% @doc RPC handler.
-module(message_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include_lib("include/syncshare.hrl").

init(_Transport, Req, Opts) ->
    Channel = proplists:get_value(channel, Opts),
	{ok, Req, {amqp_channel, Channel}}.

handle(Req, {amqp_channel, Channel}=State) ->
    {Service, _} = cowboy_req:binding(service, Req),
    {Queue, _}   = cowboy_req:binding(queue, Req),
    {Call, _}    = cowboy_req:binding(message, Req),

    {ok, [{<<"body">>, Body}], Req2} = cowboy_req:body_qs(Req),

    syncshare_amqp:call(Channel, Queue, #payload{service=Service, call=Call, body=Body}),

    {ok, Req3} = cowboy_req:reply(200, [], <<"ok">>, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

