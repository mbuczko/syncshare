%% @private
-module(syncshare_amqp).

%% API.
-export([init/0]).
-export([init_queue/2, init_exchanges/2]).
-export([listen/2, terminate/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

init() ->
    Params = #amqp_params_network{host = "localhost"},
	{ok, Connection} = amqp_connection:start(Params),
	{ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Connection, Channel}.

init_queue(Channel, Service) ->
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),
    
    Binding = #'queue.bind'{queue = Queue, exchange = Service},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    {ok, Queue}.

init_exchanges(Channel, E) ->
    lists:foreach(fun(X) ->
                          declare_public_exchange(Channel, X),
                          declare_private_exchange(Channel, X)
                  end, E).

declare_public_exchange(Channel, X) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<X/binary, "/public">>, type = <<"fanout">>}).

declare_private_exchange(Channel, X) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = <<X/binary, "/private">>, type = <<"direct">>}).


listen(Channel, Q) ->
    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(Channel, Sub).


terminate(Connection, Channel) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
	ok.

