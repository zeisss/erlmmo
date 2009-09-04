%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(ewc).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the ewc server.
start() ->
    ewc_deps:ensure(),
    ensure_started(crypto),
    application:start(ewc).

%% @spec stop() -> ok
%% @doc Stop the ewc server.
stop() ->
    Res = application:stop(ewc),
    application:stop(crypto),
    Res.
