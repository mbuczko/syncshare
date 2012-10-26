%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(init_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).


init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Val, _} = cowboy_req:qs_val(<<"service">>, Req),
	{ok, Req2} = cowboy_req:reply(200, [], io_lib:format("<script>var es=new EventSource('/syncshare/sse/~s'); es.addEventListener('msg',function(msg) { window.parent.postMessage({reply: msg.data}, '*'); });</script>", [Val]), Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.

