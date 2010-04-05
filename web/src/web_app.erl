%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the web application.

-module(web_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for web.
start(_Type, _StartArgs) ->
    web_deps:ensure(),
    web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for web.
stop(_State) ->
    ok.
