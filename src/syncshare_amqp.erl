%% @private
-module(syncshare_amqp).

%% API.
-export([init/0]).
-export([init_queue/2, declare_exchange/3, declare_exchanges/2, ack/2, call/3]).
-export([cancel_subscription/2, delete_queue/2]).
-export([listen/2, terminate/2]).

-include_lib("include/syncshare.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

init() ->
    Params = #amqp_params_network{host = "localhost"},
	{ok, Connection} = amqp_connection:start(Params),
	{ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Connection, Channel}.

init_queue(Channel, Service) ->
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),
    
    % bind to 'public' exchange
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = <<Service/binary, "-public">>}),

    % bind to 'private' exchange
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = <<Service/binary, "-private">>}),

    {ok, Queue}.

declare_exchange(Channel, X, Scope) ->
    {Suffix, Type} = case Scope of
                       rpc ->     {<<"rpc">>,     <<"topic">> };
                       public ->  {<<"public">>,  <<"fanout">>};
                       private -> {<<"private">>, <<"direct">>}
                   end,
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<X/binary, "-", Suffix/binary>>, type = Type}).

declare_exchanges(Channel, E) ->
    lists:foreach(fun(X) ->
                          declare_exchange(Channel, X, rpc),
                          declare_exchange(Channel, X, public),
                          declare_exchange(Channel, X, private)
                  end, E).

delete_queue(Channel, Queue) ->
    Delete = #'queue.delete'{queue = Queue},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete).

listen(Channel, Queue) ->
    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(Channel, Sub).

call(Channel, Queue, #payload{service=Service, call=Call, body=Body}) ->
    % generate uuid as correlation id
    Uuid = ossp_uuid:make(v4, text),
    QName = <<"amq.gen-", Queue/binary>>,
    Props = #'P_basic'{correlation_id=Uuid, reply_to=QName},

    io:format("Queue=~s  Call=~s  Body=~p~n", [QName, Call, Body]),

    Publish = #'basic.publish'{exchange = <<Service/binary, "-rpc">>, routing_key = Call},
    amqp_channel:cast(Channel, Publish, #amqp_msg{props=Props, payload=Body}),
    {ok, Uuid}.

ack(Channel, Tag) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).

cancel_subscription(Channel, Tag) ->
    amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = Tag}).

terminate(Connection, Channel) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
	ok.

