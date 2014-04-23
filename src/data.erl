-module(data).

-behavior(e2_service).

-export([start_link/1, get/1, get/2, put/3, del/1]).

-export([init/1, handle_msg/3]).

start_link(File) ->
    e2_service:start_link(?MODULE, File, [registered]).

get(Key) ->
    e2_service:call(?MODULE, {get, Key}).

get(Key, Default) ->
    e2_service:call(?MODULE, {get, Key, Default}).

put(Key, Value, Expire) ->
    e2_service:call(?MODULE, {put, Key, Value, Expire}).

del(Key) ->
    e2_service:call(?MODULE, {del, Key}).


init(File) ->
    {ok, open_db(File)}.

open_db(File) ->
    {ok, Db} = store:open(File), Db.


handle_msg({get, Key}, _From, Db) ->
    {reply, store:get(Db, Key), Db};

handle_msg({get, Key, Default}, _From, Db) ->
    {reply, store:get(Db, Key, Default), Db};

handle_msg({put, Key, Value, Expire}, _From, Db) ->
    {reply, store:put(Db, Key, Value, Expire), Db};

handle_msg({del, Key}, _From, Db) ->
    {reply, store:del(Db, Key), Db}.
