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
          load     :: string()}).

get_header(Name, Headers, Default) ->
	case lists:keyfind(Name, 1, Headers) of
		false -> { ok, Default };
		{Name, _, Value} -> {ok, Value}
    end.
