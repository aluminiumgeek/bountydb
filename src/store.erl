-module(store).

-export([open/1, put/4, get/2, get/3, del/2]).

open(File) ->
    create_bloom_filter(),
    
    {_, Db} = dets:open_file(File, []),
    fill_bloom_filter(Db),
    
    {ok, Db}.

put(Db, Key, Value, Expire) ->
    Bloom = get_bloom(),
    case bloom:is_element(Key, Bloom) of
        true ->
            exists;
        false ->
            ets:insert(bloom_filter, {first, bloom:add_element(Key, Bloom)})
    end,
    
    case Expire of
        0 ->
            Expiration = 0;
        Append ->
            Expiration = utils:get_unixtime() + Append
    end,

    dets:insert(Db, {Key, {Value, Expiration}}).

get(Db, Key) ->
    Bloom = get_bloom(),
    case bloom:is_element(Key, Bloom) of
        true ->
            handle_store_lookup(dets:lookup(Db, Key));
        false ->
            handle_store_lookup([])
    end.

get(Db, Key, Default) ->
    Bloom = get_bloom(),
    case bloom:is_element(Key, Bloom) of
        true ->
            handle_store_lookup(dets:lookup(Db, Key));
        false ->
            handle_store_lookup({ok, Default})
    end.

del(Db, Key) ->
    dets:delete(Db, Key).


handle_store_lookup([{_, Entry}]) ->
    {Value, Expiration} = Entry,
    
    case Expiration of
        0 ->
            {ok, Value};
        Timestamp ->
            case utils:get_unixtime() =< Timestamp of
                true ->
                    {ok, Value};
                false ->
                    error
            end
    end;

handle_store_lookup({_, Default}) -> {ok, Default};
handle_store_lookup([]) -> error.
%handle_store_lookup([{_, Value}], _) -> {ok, Value};


create_bloom_filter() ->
    ets:new(bloom_filter, [set, public, named_table]),
    ets:insert(bloom_filter, {first, bloom:new(2000)}).

fill_bloom_filter(Db) ->
    lists:foreach(fun(N) ->
                      Bloom = get_bloom(),
                      [{Key, _}] = N,
                      ets:insert(bloom_filter, {first, bloom:add_element(Key, Bloom)})
                  end, dets:match(Db, '$1')).

get_bloom() ->
    [{_, Bloom}] = ets:lookup(bloom_filter, first),
    Bloom.
