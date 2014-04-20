-module(server).

-export([init/1]).

init(Port) ->
    {ok, listen(Port)}.

listen(Port)->
    misultin:start_link([
        {port, Port}, 
        {loop, fun(Req) -> server_rest_handler:handle_http(Req) end}
    ]).

stop() ->
    misultin:stop().