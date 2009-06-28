-module(main_app).
%%%-------------------------------------------------------------------
%%% File    : main_app.erl
%%% Author  : Stephan Zeissler
%%% Description :
%%%  Starts and stops the main supervisor.
%%%
%%%-------------------------------------------------------------------
-behaviour(application).

-include("erlmmo.hrl").

-export([
     start/0,
     start/2,
     stop/0,
     stop/1,
     start_phase/3,
     prep_stop/1,
     config_change/3
     ]).

start() ->
    start(normal, []).
    
start(_Type, _Args) ->
	% TODO: Parse the arguments and update the configuration
	Config = #app_config{},
	main_sup:start_link(Config).

start_phase(_Phase, _StartType, _PhaseArgs) -> ok.

prep_stop(State) -> State.
	
stop() -> 
    stop([]).
stop(_State) ->
	main_sup:stop().
	
config_change(_Changed, _New, _Removed) -> ok.


