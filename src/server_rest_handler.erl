-module(server_rest_handler).

-export([handle_http/1]).

handle_http(Req) ->
    handle(
        Req:get(method),
        Req:resource([lowercase, urldecode]),
        Req
    ).

handle('GET', [], Req) ->
    handle_reply({ok, "API is running"}, Req);

handle('GET', ["store", Key], Req) ->
    handle_reply(store_get(Key), Req);

handle('POST', ["store", Key], Req) ->
    case lists:filter(fun({Param, _}) -> string:equal(Param, "value") end, Req:parse_post()) of
        [{_, Value}] ->
            handle_reply(store_put(Key, Value), Req);
        [] ->
            handle_reply(error, Req)
    end.
    


store_get(Key) ->
    data:get(Key).

store_get(Key, Default) ->
    data:get(Key, Default).

store_put(Key, Value) ->
    data:put(Key, Value).

store_del(Key) ->
    data:del(Key).


handle_reply({ok, Value}, Req) ->
    Req:ok([{"Content-Type", "text/plain"}], Value);
handle_reply(ok, Req) ->
    Req:ok([{"Content-Type", "text/plain"}], "ok");
handle_reply(error, Req) ->
    Req:ok([{"Content-Type", "text/plain"}], "error").
