-module(store).

-export([open/1, put/3, get/2, get/3, del/2]).

open(File) ->
    dets:open_file(File, []).

put(Db, Key, Value) ->
    dets:insert(Db, {Key, Value}).

get(Db, Key) ->
    handle_store_lookup(dets:lookup(Db, Key)).

get(Db, Key, Default) ->
    handle_store_lookup(dets:lookup(Db, Key), Default).

del(Db, Key) ->
    dets:delete(Db, Key).


handle_store_lookup([{_, Value}]) -> {ok, Value};
handle_store_lookup([]) -> error.
handle_store_lookup([{_, Value}], _) -> {ok, Value};
handle_store_lookup([], Default) -> {ok, Default}.
