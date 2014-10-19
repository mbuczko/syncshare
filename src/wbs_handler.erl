-module(wbs_handler).  
-behaviour(cowboy_websocket_handler).  

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-import(syncshare_utils, [get_header/3, memoize/3]).

-include_lib("include/syncshare.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, Opts) ->
    Channel = proplists:get_value(channel, Opts),

    {Service, _} = cowboy_req:binding(service, Req),
    {Cookie, _}  = cowboy_req:cookie(<< ?COOKIE_NAME >>, Req),

    % decode cookie
    {ok, {_, Token}} = termit:decode_base64(Cookie, ?COOKIE_SECRET),

	% create queue with initial name
    {ok, Queue} = syncshare_amqp:init_queue(<<>>, Channel, Service, 0),

    % and start listening...
    syncshare_amqp:listen(Channel, Queue),

    lager:info("Initializing WEBSOCKETS: service=~s with queue=~p and token~p~n", [Service, Queue, Token]),
	{ok, Req, #state{service=Service, token=Token, amqp_channel=Channel, amqp_queue=Queue}}.

websocket_handle({text, Msg}, Req, #state{service=Service, token=Token, amqp_channel=Channel, amqp_queue=Queue}=State) ->
    [Fn|Data] = re:split(Msg, " ", [{return, binary}, {parts, 2}]),

    syncshare_amqp:call(Channel, Queue, #payload{service=Service, call=Fn, token=Token, data=list_to_binary(Data)}),
	{ok, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info(#'basic.consume_ok'{consumer_tag=Tag}, Req, #state{service=Service, token=Token, amqp_channel=Channel, amqp_queue=Queue}=State) ->

	% if token was given, let's push authorization request
	%% syncshare_amqp:authorize(Channel, Queue, Service, Token),

    lager:info("basic.consume ~p~n", [Tag]),
	{ok, Req, State#state{consumer_tag=Tag}};

websocket_info(#'basic.cancel_ok'{}, Req, State) ->
    lager:info("basic.cancel_ok...~n"),
	{ok, Req, State};

websocket_info(#'basic.cancel'{}, Req, State) ->
    lager:info("basic.cancel...~n"),
	{ok, Req, State};

websocket_info({#'basic.deliver'{delivery_tag=Tag}, Content}, Req, #state{amqp_channel=Channel}=State) ->
    #amqp_msg{payload = Payload, props = #'P_basic'{headers = Headers, correlation_id = Id}} = Content,

    % acknowledge incoming message
    syncshare_amqp:ack(Channel, Tag),

    % get type of message (message / broadcast) and called function name
    {ok, Type} = get_header(<<"type">>, Headers, <<"broadcast">>),

	% get back called function name (stored in correlation id)
    [Call|_] = string:tokens(binary_to_list(Id), "-"),

    lager:info("basic.deliver (~s/~s)~n", [Call, Type]),
    {reply, {text, [Call, " ", Type, " ", Payload]}, Req, memoize(Call, State, Payload)};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{amqp_channel=Channel, consumer_tag=Tag}=_State) ->
    lager:info("Terminating with tag: ~p...~n", [Tag]),
    syncshare_amqp:cancel_subscription(Channel, Tag),
	ok.
