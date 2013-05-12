-module(syncshare_utils).
-export([get_header/3]).
-export([cookie_string/2]).

-include_lib("include/syncshare.hrl").

get_header(Name, Headers, Default) ->
	case lists:keyfind(Name, 1, Headers) of
		false -> { ok, Default };
		{Name, _, Value} -> {ok, Value}
    end.
    
cookie_string(Service, Data) ->
    << ?COOKIE_NAME, "=", Data/binary, ";path=", ?COOKIE_PATH, Service/binary, ";HttpOnly" >>.

