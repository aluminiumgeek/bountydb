-module(utils).

-export([get_unixtime/0]).

get_unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
