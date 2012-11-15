%% @doc Hello world handler.
-module(sse_handler).

-export([init/3, info/3]).
-export([terminate/2]).

-include_lib("include/syncshare.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(TIMEOUT, 60000).

init({tcp, http}, Req, Opts) ->
    Channel =  proplists:get_value(channel, Opts),

    {Service, _} = cowboy_req:binding(service, Req),
    {Timeout, _} = cowboy_req:qs_val(<<"timeout">>, Req, ?TIMEOUT),
    {Cookie,  _} = cowboy_req:cookie(<<"_syncshare">>, Req),

    {ok, Queue} = syncshare_amqp:init_queue(Cookie, Channel, Service, Timeout*2),

    io:format("Initializing connection to service: '~s' with queue=~p~n", [Service, Queue]),

    syncshare_amqp:listen(Channel, Queue),
    {loop, Req, #state{service=Service, amqp_channel=Channel, amqp_queue=Queue}, Timeout, hibernate}.


info(#'basic.consume_ok'{consumer_tag=Tag}, Req, #state{service=Service, amqp_queue=Queue}=State) ->
    {ok, Transport, Socket} = cowboy_req:transport(Req),
    {Version, _} = cowboy_req:version(Req),

    HTTPVer = cowboy_http:version_to_binary(Version),
    Status  = << HTTPVer/binary, " 200 OK\r\n" >>,
    Type    = << "Content-Type: text/event-stream\r\n" >>,
    Cookie  = << "Set-Cookie: _syncshare=" >>,
    Value   = binary_to_list(Queue),

    io:format("basic.consume ~p~n", [Tag]),

    Event = ["event: connected\ndata: ", Value, "\n\n"],
    Transport:send(Socket, [Status, Type, Cookie, Value, <<";path=/syncshare/">>, Service, <<";HttpOnly\r\n\r\n">>, Event]),

	{loop, Req, State#state{consumer_tag=Tag}, hibernate};

info({#'basic.deliver'{delivery_tag=Tag}, Content}, Req, #state{amqp_channel=Channel}=State) ->
    {ok, Transport, Socket} = cowboy_req:transport(Req),

    #amqp_msg{payload = Payload, props = #'P_basic'{headers = Headers, priority = _Priority}} = Content,

    % acknowledge incoming message
    syncshare_amqp:ack(Channel, Tag),

    % get type of message (rpc/public)
    {ok, Type} = get_header(<<"type">>, Headers, <<"public">>),

    Event = ["event: ", Type, "\ndata: ", Payload, "\n\n"],
    Transport:send(Socket, Event),

    {loop, Req, State, hibernate};

info(#'basic.cancel_ok'{}, Req, State) ->
    io:format("basic.cancel_ok...~n"),
	{loop, Req, State, hibernate};

info(#'basic.cancel'{}, Req, State) ->
    io:format("basic.cancel...~n"),
	{loop, Req, State, hibernate};

info(_Message, Req, State) ->
	{loop, Req, State, hibernate}.


terminate(_Req, #state{amqp_channel=Channel, consumer_tag=Tag}=_State) ->
    io:format("Terminating with tag: ~p...~n", [Tag]),
    syncshare_amqp:cancel_subscription(Channel, Tag),
    ok.

get_header(Name, Headers, Default) ->
	case lists:keyfind(Name, 1, Headers) of
		false -> { ok, Default };
		{Name, _, Value} -> {ok, Value}
    end.
