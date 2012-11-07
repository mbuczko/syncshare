%% handler state
-record(state, {
          service      :: binary(),
          consumer_tag :: any(),
          delivery_tag :: any(),
          amqp_channel :: any(),
          amqp_queue   :: any()}).

%% RPC payload
-record(payload, {
          service  :: string(),
          call     :: string(),
          body     :: string()}).

