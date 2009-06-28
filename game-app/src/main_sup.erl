%%%--------------------------------------------------------------------------
%%% File    : main_sup.erl
%%% Author  : Stephan Zeissler
%%% Description :
%%%  This module is a supervisor starting a number of child supervisors.
%%%
%%% Children:
%%%  ??
%%%
%%%--------------------------------------------------------------------------
-module(main_sup).
-behaviour (supervisor).
-include("erlmmo.hrl").
%% API
-export([start_link/1, stop/0]).

%% Supervisor Callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Config=#app_config{}) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).
	
%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: stop() -> true
%% Description: Stops the supervisor
%%--------------------------------------------------------------------
stop() ->
	Pid = whereis(?MODULE),
	case Pid of
		undefined -> 
			not_started;
		_ -> 
			true = exit(Pid, shutdown),
			ok
	end.
	
%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([Config]) ->
    
    %TcpHandler = {},
    %TcpSup = {},
	LoginSup = {login_server, {login_server, start_link, []}, permanent, 10, worker, []},
	PlayerSup = {player_sup, {player_sup, start_link, []}, permanent, 10, supervisor, []},
	
    % Forks a number of zones
    ZoneSup = {zone_sup, {zone_sup, start_link, [{zone_files, Config#app_config.zone_files}]}, permanent, 10, supervisor, []},
    
	ChildSpec = [
	   % TcpListener, 
	   % TcpSup,
	   %LoginSup,
	   %PlayerSup,
	   % WorldMap,
	   ZoneSup
	],

	{ok, {{rest_for_one, 3, 30}, ChildSpec}}.
	
	
	