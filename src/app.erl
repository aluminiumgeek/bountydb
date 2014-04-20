-module(app).

-behavior(e2_application).

-export([init/0]).

-define(DEFAULT_DB_FILE, "/tmp/data.db").
-define(DEFAULT_PORT, 3334).

init() ->
    {ok, [
        {data, start_link, [database_file()]},
        {server, start_link, [server_port()]}
    ]}.

database_file() ->
    app_config(database_file, ?DEFAULT_DB_FILE).

server_port() ->
    app_config(server_port, ?DEFAULT_PORT).

app_config(Name, Default) ->
    handle_app_env(application:get_env(Name), Default).

handle_app_env({ok, Value}, _Default) -> Value;
handle_app_env(undefined, Default) -> Default.