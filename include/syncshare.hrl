%% handler state
-record(state, {
          service      :: binary(),
          session      :: any(),
          consumer_tag :: any(),
          delivery_tag :: any(),
          amqp_channel :: any(),
          amqp_queue   :: any()}).

%% Message payload
-record(payload, {
          service  :: string(),
          call     :: string(),
          load     :: string()}).

%% SSE session params
-define(COOKIE_NAME,   "_syncshare").
-define(COOKIE_PATH,   "/syncshare/sse/").
-define(COOKIE_SECRET, "D9fG7qkcDwUUYJ").
