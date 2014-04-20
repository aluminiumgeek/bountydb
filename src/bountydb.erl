-module(bountydb).

-export([start/0, stop/0]).

start() ->
    e2_application:start_with_dependencies(bountydb).

stop() ->
    io:format('Stopping database~n'),
    application:stop(bountydb).
