-module(server_rest_handler).

-export([handle_http/1, handle/3]).

handle_http(Req) ->
    handle(
        Req:get(method),
        Req:resource([lowercase, urldecode]),
        Req
    ).

handle('GET', [], Req) ->
    Req:ok([{"Content-Type", "text/plain"}], "Test.").