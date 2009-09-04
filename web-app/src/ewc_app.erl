%% @copyright 2009 Stephan Zeissler.
%% @doc Callbacks for the Erlang-Web-Chat application.

-module(ewc_app).
-author('Stephan Ziessler <zeisss@moinz.de>').

-behaviour(application).
-export([start/2,stop/1]).

%%
%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for skel.
start(_Type, _StartArgs) ->
    ewc_deps:ensure(),
    ewc_sup:start_link().

%%
%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for skel.
stop(_State) ->
    ok.
