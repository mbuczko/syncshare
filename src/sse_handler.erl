%% @doc Hello world handler.
-module(sse_handler).

-export([init/3, info/3]).
-export([terminate/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(TIMEOUT, 60000).

%% handler state
-record(state, {
          consumer_tag :: any(),
          delivery_tag :: any(),
          amqp_channel :: any(),
          amqp_queue   :: any()}).


init({tcp, http}, Req, Opts) ->
    Channel =  proplists:get_value(channel, Opts),
    {Service, _} = cowboy_req:path_info(Req), 

    io:format("Initializing connection to service: ~p~n", [Service]),

    % create amqp queue
    {ok, Queue} = syncshare_amqp:init_queue(Channel, list_to_binary([Service, "/public"])),

    % bind queue with current process
    syncshare_amqp:listen(Channel, Queue),

	{loop, Req, #state{amqp_channel=Channel, amqp_queue=Queue}, ?TIMEOUT, hibernate}.

info(#'basic.consume_ok'{consumer_tag=Tag}, Req, State) ->
    io:format("basic.consume ~p~n", [Tag]),
	{loop, Req, State#state{consumer_tag=Tag}, hibernate};

info({#'basic.deliver'{delivery_tag=Tag}, Content}, Req, #state{amqp_channel=Channel, delivery_tag=T}=State) ->
    {ok, Transport, Socket} = cowboy_req:transport(Req),

    #amqp_msg{payload = Payload} = Content,

    % acknowledge incoming message
    syncshare_amqp:ack(Channel, Tag),

    Event = ["data: ", Payload, "\n\n"],
    case T of
        undefined -> {Version, _} = cowboy_req:version(Req),
                     HTTPVer = cowboy_http:version_to_binary(Version),
                     Status = << HTTPVer/binary, " 200 OK\r\n" >>,
                     Type = << "Content-Type: text/event-stream\r\n" >>,
                     Reply = [Status, Type, <<"\r\n">>, Event];
        T -> Reply = Event
    end,

    Transport:send(Socket, Reply),
    {loop, Req, State#state{delivery_tag=Tag}, hibernate};

info(#'basic.cancel_ok'{}, Req, State) ->
    io:format("basic.cancel_ok...~n"),
	{loop, Req, State, hibernate};

info(#'basic.cancel'{}, Req, State) ->
    io:format("basic.cancel...~n"),
	{loop, Req, State, hibernate};

info(Message, Req, State) ->
	{loop, Req, State, hibernate}.


terminate(_Req, #state{amqp_channel=Channel, amqp_queue=Queue, consumer_tag=Tag}=_State) ->
    io:format("Terminating with tag: ~p...~n", [Tag]),

    % remove amqp queue
    syncshare_amqp:cancel_subscription(Channel, Tag),
    syncshare_amqp:delete_queue(Channel, Queue),
    ok.
