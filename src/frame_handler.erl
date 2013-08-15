-module(frame_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-import(syncshare_utils, [cookie_string/2]).

-include_lib("include/syncshare.hrl").

init(_Transport, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
    {Service, _} = cowboy_req:binding(service, Req),
    {Token, _} = cowboy_req:qs_val(<<"token">>, Req, ""),

    lager:info("Rendering for service ~p", [Service]),

    {ok, HTML} = frame_dtl:render([{service, Service}, {token, Token}]),

    % setup an initial session in encrypted cookie
    Encoded = termit:encode_base64(<<>>, ?COOKIE_SECRET),
    Cookie  = cookie_string(Service, Encoded),

	{ok, Req2} = cowboy_req:reply(200, [{<<"Set-Cookie">>, Cookie}], HTML, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

