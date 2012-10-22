%% @doc Hello world handler.
-module(sse_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% handler state
-record(state, {
	amqp_connection :: binary(),
	amqp_channel :: binary() }).


init(_Transport, Req, _Opts) ->
    Connection =  proplists:get_value(connection, _Opts),
    Channel =  proplists:get_value(channel, _Opts),

	{ok, Req, #state{amqp_connection=Connection, amqp_channel=Channel}}.

handle(Req, #state{amqp_channel=Channel}=State) ->
    {Service, _} = cowboy_req:path_info(Req), 

    % create amqp queue
    {ok, Queue} = syncshare_amqp:init_queue(Channel, list_to_binary([Service, "/public"])),

    % bind queue with current process
    syncshare_amqp:listen(Channel, Queue),

    {ok, Req2} = cowboy_req:chunked_reply(200, [{<<"Content-Type">>, <<"text/event-stream">>}], Req),

    handle_loop(Req2, State).

handle_loop(Req, State) -> 
    receive 
        shutdown -> 
            {ok, Req, State};
        {#'basic.deliver'{delivery_tag = Tag}, Content} ->
            #amqp_msg{payload = Payload} = Content,
            Event = ["data: ", Payload, "\n\n"], 
            ok = cowboy_req:chunk(Event, Req), 
            handle_loop(Req, State);
        {event, Message} -> 
            Event = ["data: ", Message, "\n\n"], 
            ok = cowboy_req:chunk(Event, Req), 
            handle_loop(Req, State)
    end.

terminate(_Req, _State) -> ok.
