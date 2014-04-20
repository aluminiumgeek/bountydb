-module(server).

-behavior(e2_service).

-export([start_link/1]).

-export([init/1, stop/0]).

start_link(Port) ->
    e2_service:start_link(?MODULE, Port, [registered]).

init(Port) ->
    {ok, listen(Port)}.

listen(Port)->
    misultin:start_link([
        {port, Port}, 
        {loop, fun(Req) -> server_rest_handler:handle_http(Req) end}
    ]).

stop() ->
    misultin:stop().