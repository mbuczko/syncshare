%% handler state
-record(state, {
          service      :: binary(),
		  token        :: binary(),
		  belongs      :: binary(),
          consumer_tag :: any(),
          delivery_tag :: any(),
          amqp_channel :: any(),
          amqp_queue   :: any()}).

%% Message payload
-record(payload, {
          service  :: string(),
          call     :: string(),
          data     :: string(),
          token    :: string()}).

%% How many broadcast responses to cache
-define(KEEP_MSGS, 10).

%% SSE session params
-define(COOKIE_NAME,   "_syncshare").
-define(COOKIE_PATH,   "/syncshare/").
-define(COOKIE_SECRET, "D9fG7qkcDwUUYJ").
