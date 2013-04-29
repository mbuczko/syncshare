%% @private
-module(syncshare_amqp).

%% API.
-export([init/0]).
-export([init_queue/3, declare_exchange/3, declare_exchanges/2, ack/2, call/3]).
-export([cancel_subscription/2, delete_queue/2]).
-export([listen/2, terminate/2]).

-include_lib("include/syncshare.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

init() ->
    Params = #amqp_params_network{host = "localhost"},
	{ok, Connection} = amqp_connection:start(Params),
	{ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Connection, Channel}.

init_queue(Channel, Service, Timeout) ->
    Q = queue_name(),

    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{queue = Q, arguments = [{<<"x-expires">>, long, Timeout}]}),
    
    % bind to 'public' exchange
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = <<Service/binary, "-public">>}),

    % bind to 'direct' exchange
    #'queue.bind_ok'{} = amqp_channel:call(Channel, #'queue.bind'{queue = Queue, exchange = <<Service/binary, "-direct">>,  routing_key = Queue}),

    {ok, Queue}.

declare_exchange(Channel, X, Scope) ->
    {Suffix, Type} = case Scope of
                       public -> {<<"public">>, <<"fanout">>};
                       direct -> {<<"direct">>, <<"topic">>}
                   end,
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<X/binary, "-", Suffix/binary>>, type = Type}).

declare_exchanges(Channel, E) ->
    lists:foreach(fun(X) ->
                          declare_exchange(Channel, X, public),
                          declare_exchange(Channel, X, direct)
                  end, E).

delete_queue(Channel, Queue) ->
    Delete = #'queue.delete'{queue = Queue},
    #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete).

listen(Channel, Queue) ->
    Sub = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{} = amqp_channel:call(Channel, Sub).

call(Channel, Queue, #payload{service=Service, call=Call, body=Body}) ->
    % generate uuid as correlation id
    Uuid = ossp_uuid:make(v4, text),
    Props = #'P_basic'{correlation_id=Uuid, reply_to=Queue},

    Publish = #'basic.publish'{exchange = <<Service/binary, "-direct">>, routing_key = Call},
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

queue_name(<<>>) -> queue_name();
queue_name(undefined) -> queue_name();
queue_name(Name) -> Name.

queue_name() ->
    random:seed(erlang:now()),
    list_to_binary(["gen-", random_string(16)]).

random_string(Len) ->
    Chrs = list_to_tuple("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"),
    F = fun(_, R) -> [element(random:uniform(size(Chrs)), Chrs) | R] end,
    lists:foldl(F, "", lists:seq(1, Len)).
