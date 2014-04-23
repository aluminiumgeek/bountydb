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
    case proplists:get_value("default", Req:parse_qs()) of
        undefined ->
            handle_reply(store_get(Key), Req);
        Default ->
            handle_reply(store_get(Key, Default), Req)
    end;

handle('PUT', ["store", Key], Req) ->
    Params = element(1, jiffy:decode(Req:get(body))),
    
    case proplists:get_value(<<"value">>, Params) of
        undefined ->
            handle_reply(error, Req);
        Value ->
            case proplists:get_value(<<"timeout">>, Params) of
                undefined ->
                    Expire = 0;
                Timeout ->
                    Expire = Timeout
            end,
            
            handle_reply(store_put(Key, Value, Expire), Req)
    end;

handle('DELETE', ["store", Key], Req) ->
    handle_reply(store_del(Key), Req);

handle(_, _, Req) ->
    template(Req, {[{status, error}]}).


store_get(Key) ->
    data:get(Key).

store_get(Key, Default) ->
    data:get(Key, Default).

store_put(Key, Value, Expire) ->
    data:put(Key, Value, Expire).

store_del(Key) ->
    data:del(Key).


handle_reply({ok, Value}, Req) ->
    template(Req, {[{status, ok}, {value, unicode:characters_to_binary(Value)}]});
handle_reply(ok, Req) ->
    template(Req, {[{status, ok}]});
handle_reply(error, Req) ->
    template(Req, {[{status, error}]}).


template(Req, Content) ->
    Req:ok([{"Content-Type", "application/json"}], jiffy:encode(Content)).