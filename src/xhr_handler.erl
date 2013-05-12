%% @doc XHR handler.
-module(xhr_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include_lib("include/syncshare.hrl").

init(_Transport, Req, Opts) ->
    Channel = proplists:get_value(channel, Opts),
	{ok, Req, {amqp_channel, Channel}}.

handle(Req, {amqp_channel, Channel}=State) ->
    {Service, _} = cowboy_req:binding(service, Req),
    {Cookie, _}  = cowboy_req:cookie(<< ?COOKIE_NAME >>, Req),
    {Call, _}    = cowboy_req:binding(call, Req),

    % extract payload from POST request
    {ok, [{<<"payload">>, Payload}], Req2} = cowboy_req:body_qs(Req),

    % decode cookie
    {ok, Session}  = termit:decode_base64(Cookie, ?COOKIE_SECRET),
    {QName, Token} = Session,

    syncshare_amqp:call(Channel, QName, #payload{service=Service, call=Call, load=Payload}),

    {ok, Req3} = cowboy_req:reply(200, [], <<"ok">>, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

