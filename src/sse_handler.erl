%% @doc Hello world handler.
-module(sse_handler).

-export([init/3, info/3]).
-export([terminate/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(TIMEOUT, 60000).

%% handler state
-record(state, {
	amqp_connection :: binary(),
	amqp_channel :: binary() }).


init(_Transport, Req, _Opts) ->
    Connection =  proplists:get_value(connection, _Opts),
    Channel =  proplists:get_value(channel, _Opts),

    io:format("Initializing connection~n"),

    {Service, _} = cowboy_req:path_info(Req), 

    % create amqp queue
    {ok, Queue} = syncshare_amqp:init_queue(Channel, list_to_binary([Service, "/public"])),

    % bind queue with current process
    syncshare_amqp:listen(Channel, Queue),

	{loop, Req, #state{amqp_connection=Connection, amqp_channel=Channel}, ?TIMEOUT, hibernate}.

info(#'basic.consume_ok'{}=Message, Req, State) ->
    io:format("~p~n", [Message]),
	{loop, Req, State, hibernate};

info({#'basic.deliver'{delivery_tag = Tag}, Content} = Message, Req, State) ->
    #amqp_msg{payload = Payload} = Content,
    Event = ["data: ", Payload, "\n\n"],
    
    {ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/event-stream">>}], Event, Req), 
	{ok, Req2, State}.

terminate(_Req, _State) ->
    io:format("Terminating...~n"),
    ok.
