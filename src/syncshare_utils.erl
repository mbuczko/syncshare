-module(syncshare_utils).
-export([get_header/3]).
-export([cookie_string/3]).
-export([memoize/3]).

-include_lib("include/syncshare.hrl").

get_header(Name, Headers, Default) ->
	case lists:keyfind(Name, 1, Headers) of
		false -> { ok, Default };
		{Name, _, Value} -> {ok, Value}
    end.

memoize(<<"authenticate">>, State, Payload) ->
	State#state{belongs=Payload};

memoize(_, State, _) ->
	State.

cookie_string(Service, Trans, Data) ->
	Path = cookie_path(Trans),
    << ?COOKIE_NAME, "=", Data/binary, ";path=", ?COOKIE_PATH, Path/binary, Service/binary, ";HttpOnly" >>.

cookie_path(<<"sse">>) -> <<"sse/">>;
cookie_path(_) -> <<"wbs/">>.
