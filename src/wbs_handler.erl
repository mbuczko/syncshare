-module(wbs_handler).  
-behaviour(cowboy_websocket_handler).  

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-import(syncshare_utils, [get_header/3]).

-include_lib("include/syncshare.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, Opts) ->
    Channel =  proplists:get_value(channel, Opts),
    {Service, _} = cowboy_req:binding(service, Req),

    {ok, Queue} = syncshare_amqp:init_queue(<<>>, Channel, Service, 0),

    lager:info("Initializing WEBSOCKET connection to service: '~s' with queue=~p~n", [Service, Queue]),

    syncshare_amqp:listen(Channel, Queue),
	{ok, Req, #state{service=Service, amqp_channel=Channel, amqp_queue=Queue}}.

websocket_handle({text, Msg}, Req, #state{service=Service, amqp_channel=Channel, amqp_queue=Queue}=State) ->
    [Call|Body] = re:split(Msg, "\\|", [{return, binary}, {parts, 3}]),
    [Token|Payload] = Body,

    syncshare_amqp:call(Channel, Queue, #payload{service=Service, call=Call, token=Token, load=list_to_binary(Payload)}),
	{ok, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info(#'basic.consume_ok'{consumer_tag=Tag}, Req, State) ->
    lager:info("basic.consume ~p~n", [Tag]),
	{ok, Req, State#state{consumer_tag=Tag}};

websocket_info(#'basic.cancel_ok'{}, Req, State) ->
    lager:info("basic.cancel_ok...~n"),
	{ok, Req, State};

websocket_info(#'basic.cancel'{}, Req, State) ->
    lager:info("basic.cancel...~n"),
	{ok, Req, State};

websocket_info({#'basic.deliver'{delivery_tag=Tag}, Content}, Req, #state{amqp_channel=Channel}=State) ->
    #amqp_msg{payload = Payload, props = #'P_basic'{headers = Headers, priority = _Priority}} = Content,

    % acknowledge incoming message
    syncshare_amqp:ack(Channel, Tag),

    % get type of message (message / broadcast)
    {ok, Type} = get_header(<<"type">>, Headers, <<"broadcast">>),

    lager:info("basic.deliver (~s)~n", [Type]),
    {reply, {text, [Type, "|", Payload]}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{amqp_channel=Channel, consumer_tag=Tag}=_State) ->
    lager:info("Terminating with tag: ~p...~n", [Tag]),
    syncshare_amqp:cancel_subscription(Channel, Tag),
	ok.
