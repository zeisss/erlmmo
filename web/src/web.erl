%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(web).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    web_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    web_sup:start_link().

%% @spec start() -> ok
%% @doc Start the web server.
start() ->
    web_deps:ensure(),
    ensure_started(crypto),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(web).

%% @spec stop() -> ok
%% @doc Stop the web server.
stop() ->
    Res = application:stop(web),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
