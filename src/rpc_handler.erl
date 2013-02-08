%% @doc RPC handler.
-module(rpc_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include_lib("include/syncshare.hrl").

init(_Transport, Req, Opts) ->
    Channel = proplists:get_value(channel, Opts),
	{ok, Req, {amqp_channel, Channel}}.

handle(Req, {amqp_channel, Channel}=State) ->
    {Service, _} = cowboy_req:binding(service, Req),
    {Call, _} = cowboy_req:binding(message, Req),

    {Cookie, Req2} = cowboy_req:cookie(<<"_syncshare">>, Req),
    {ok, [{<<"body">>, Body}], Req3} = cowboy_req:body_qs(Req2),

    syncshare_amqp:call(Channel, Cookie, #payload{service=Service, call=Call, body=Body}),

    {ok, Req4} = cowboy_req:reply(200, [], <<"ok">>, Req3),
    {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
	ok.

