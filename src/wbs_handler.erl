-module(wbs_handler).  
-behaviour(cowboy_websocket_handler).  
  

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(TIMEOUT, 60000).

% Called to know how to dispatch a new connection.
init({tcp, http}, Req, _Opts) ->
    Channel =  proplists:get_value(channel, Opts),
    {Service, _} = cowboy_req:binding(service, Req),
    {Timeout, _} = cowboy_req:qs_val(<<"timeout">>, Req, ?TIMEOUT),

    {ok, Queue} = syncshare_amqp:init_queue(Channel, Service, Timeout*2),

    io:format("Initializing connection to service: '~s' with queue=~p~n", [Service, Queue]),

    syncshare_amqp:listen(Channel, Queue),

    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
