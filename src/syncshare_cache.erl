-module(syncshare_cache).

%% API.
-export([init/0, add_message/3]).

-include_lib("include/syncshare.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mesia:create_table(cache, [{attributes, record_info(fields, cached)},
                               {record_name, cached},
                               {type, set}]),
    {ok, true}.

add_message(Message, Service, <<"broadcast">>) ->
    lager:info("before cache ~s#~s~n", [Service,Message]),

    Cached = case mnesia:read(cache, Service) of
                  [] -> [];
                  [#cached{messages=Messages}] -> Messages
              end,

    %% Fun = fun() -> 
    %%               mnesia:write(#cached{service=Service, messages=[Message|lists:sublist(Cached, ?KEEP_MSGS-1)]})
    %%       end,
    %% mnesia:transaction(Fun),
    lager:info("caching ~s~n", [Cached]);

add_message(Message, Service, _) -> false.
