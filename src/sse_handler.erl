-module(sse_handler).
-behaviour(cowboy_loop_handler).

-export([init/3, info/3]).
-export([terminate/3]).

-import(syncshare_utils, [get_header/3]).
-import(syncshare_utils, [cookie_string/2]).

-include_lib("include/syncshare.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(TIMEOUT, 60000).

init({tcp, http}, Req, Opts) ->
    Channel =  proplists:get_value(channel, Opts),
    {Service, _} = cowboy_req:binding(service, Req),
    {Timeout, _} = cowboy_req:qs_val(<<"timeout">>, Req, ?TIMEOUT),
    {Cookie, Req2} = cowboy_req:cookie(<< ?COOKIE_NAME >>, Req),

    % decode cookie
    {ok, Session}  = termit:decode_base64(Cookie, ?COOKIE_SECRET),
    {QName, Token} = Session,

    % initialize AMQP queue
    {ok, Queue} = syncshare_amqp:init_queue(QName, Channel, Service, Timeout*2),

    % and start listening...
    syncshare_amqp:listen(Channel, Queue),

    lager:info("Initializing SSE connection to service: '~s' with queue=~p and token=~p~n", [Service, Queue, Token]),

    {loop, Req2, #state{service=Service, session={QName, Token}, amqp_channel=Channel, amqp_queue=Queue}, Timeout, hibernate}.

info(#'basic.consume_ok'{consumer_tag=Tag}, Req, #state{service=Service, session={QName, Token}, amqp_queue=Queue}=State) ->
    [Socket, Transport] = cowboy_req:get([socket, transport], Req),
	{Version, _} = cowboy_req:version(Req),

	HTTPVer = cowboy_http:version_to_binary(Version),
    Status  = << HTTPVer/binary, " 200 OK\r\n" >>,
    Type    = << "Content-Type: text/event-stream\r\nConnection: Keep-Alive\r\nCache-Control: no-cache\r\n" >>,

    % store new queue name in a session if it differs from existing one
    Cookie = case QName of
               Queue -> [];
               _ -> Encoded = cookie_string(Service, termit:encode_base64({Queue, Token}, ?COOKIE_SECRET)),
                    << "Set-Cookie: ", Encoded/binary, "\r\n" >>
           end,

    lager:info("basic.consume ~p~n", [Tag]),

    Event = ["event: connection\ndata: ", << Queue/binary >>, "\n\n"],
    Transport:send(Socket, [Status, Type, Cookie, <<"\r\n">>, Event]),

	{loop, Req, State#state{consumer_tag=Tag}, hibernate};

info({#'basic.deliver'{delivery_tag=Tag}, Content}, Req, #state{amqp_queue=Queue, amqp_channel=Channel}=State) ->
	[Socket, Transport] = cowboy_req:get([socket, transport], Req),

    #amqp_msg{payload = Payload, props = #'P_basic'{headers = Headers, priority = _Priority}} = Content,

    % acknowledge incoming message
    syncshare_amqp:ack(Channel, Tag),

    % get type of message (message / broadcast)
    {ok, Type} = get_header(<<"type">>, Headers, <<"broadcast">>),

    Event = ["event: ", Type, "\ndata: ", Queue, "|", Payload, "\n\n"],
    Transport:send(Socket, Event),

    lager:info("basic.deliver (~s)~n", [Type]),
    {loop, Req, State, hibernate};

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
