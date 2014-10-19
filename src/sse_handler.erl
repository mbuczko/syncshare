-module(sse_handler).
-behaviour(cowboy_loop_handler).

-export([init/3, info/3]).
-export([terminate/3]).

-import(syncshare_utils, [cookie_string/3, get_header/3, memoize/3]).

-include_lib("include/syncshare.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(TIMEOUT, 60000).

init({tcp, http}, Req, Opts) ->
    Channel =  proplists:get_value(channel, Opts),
    {Service, _} = cowboy_req:binding(service, Req),
    {Timeout, _} = cowboy_req:qs_val(<<"timeout">>, Req, ?TIMEOUT),
    {Cookie, Req2} = cowboy_req:cookie(<< ?COOKIE_NAME >>, Req),

    % decode cookie
    {ok, {QName, Token}}  = termit:decode_base64(Cookie, ?COOKIE_SECRET),

    % initialize AMQP queue
    {ok, Queue} = syncshare_amqp:init_queue(QName, Channel, Service, Timeout*2),

    % and start listening...
    syncshare_amqp:listen(Channel, Queue),

    lager:info("Initializing SERVER SIDE EVENTS: service=~s with queue=~p and token=~p", [Service, Queue, Token]),
    {loop, Req2, #state{service=Service, token=Token, amqp_channel=Channel, amqp_queue=Queue}, Timeout, hibernate}.

info(#'basic.consume_ok'{consumer_tag=Tag}, Req, #state{service=Service, token=Token, amqp_channel=Channel, amqp_queue=Queue}=State) ->
    [Socket, Transport] = cowboy_req:get([socket, transport], Req),
	{Version, _} = cowboy_req:version(Req),

	HTTPVer = cowboy_http:version_to_binary(Version),
    Status  = << HTTPVer/binary, " 200 OK\r\n" >>,
    Type    = << "Content-Type: text/event-stream\r\nConnection: Keep-Alive\r\nCache-Control: no-cache\r\n" >>,

    Encoded = cookie_string(Service, <<"sse">>, termit:encode_base64({Queue, Token}, ?COOKIE_SECRET)),
    Cookie  = << "Set-Cookie: ", Encoded/binary, "\r\n" >>,

    lager:info("basic.consume ~p~n", [Tag]),

    Event = ["event: connection\ndata: ", << Queue/binary >>, "\n\n"],
    Transport:send(Socket, [Status, Type, Cookie, <<"\r\n">>, Event]),

	% if token was given, let's push authorization request
	syncshare_amqp:authorize(Channel, Queue, Service, Token),

	{loop, Req, State#state{consumer_tag=Tag}, hibernate};

info({#'basic.deliver'{delivery_tag=Tag}, Content}, Req, #state{amqp_channel=Channel}=State) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),

    #amqp_msg{payload = Payload, props = #'P_basic'{headers = Headers, priority = _Priority, correlation_id = Id}} = Content,

    % acknowledge incoming message
    syncshare_amqp:ack(Channel, Tag),

    % get type of message (message / broadcast)
    {ok, Type} = get_header(<<"type">>, Headers, <<"broadcast">>),

	% get back called function name (stored in correlation id)
    [Call|_] = string:tokens(binary_to_list(Id), "-"),

    Event = ["data: ", Call, " ", Type, " ", Payload, "\n\n"],
    Transport:send(Socket, Event),

    lager:info("basic.deliver (~s/~s)~n", [Call, Type]),
    {loop, Req, memoize(Call, State, Payload), hibernate};

info(#'basic.cancel_ok'{}, Req, State) ->
    lager:info("basic.cancel_ok...~n"),
	{loop, Req, State, hibernate};

info(#'basic.cancel'{}, Req, State) ->
    lager:info("basic.cancel...~n"),
	{loop, Req, State, hibernate};

info(_Message, Req, State) ->
	{loop, Req, State, hibernate}.


terminate(_Reason, _Req, #state{amqp_channel=Channel, consumer_tag=Tag}=_State) ->
    lager:info("Terminating with tag: ~p...~n", [Tag]),
    syncshare_amqp:cancel_subscription(Channel, Tag),
    ok.
