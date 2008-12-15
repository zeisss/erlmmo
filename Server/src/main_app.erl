-module(main_app).
%%%-------------------------------------------------------------------
%%% File    : main_app.erl
%%% Author  : Stephan Zeissler
%%% Description :
%%%  Starts and stops the main supervisor.
%%%
%%%-------------------------------------------------------------------
-behaviour(application).

-export([
     start/2,
     stop/1
     ]).

start(_Type, _Args) ->
	main_sup:start_link().
	
stop(_State) ->
	main_sup:stop().