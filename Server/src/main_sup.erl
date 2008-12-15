%%%-------------------------------------------------------------------
%%% File    : main_sup.erl
%%% Author  : Stephan Zeissler
%%% Description :
%%%  This module is a supervisor starting a number of child supervisors.
%%%
%%% Children:
%%%  ??
%%%
%%%-------------------------------------------------------------------
-module(gs_sup).
-behaviour (supervisor).

%% API
-export([start_link/0, stop/0]).

%% Supervisor Callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
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
init([]) ->
	%TcpSup = {gs_tcp_sup, {gs_tcp_sup, start_link, []}, permanent, 10, supervisor, gs_tcp_sup:used_modules()},
	%Authenticator = {gs_authenticator, 
	%					{gs_authenticator, start_link, []}, permanent, 10, worker, 
	%					 gs_authenticator:used_modules()},
	
	% More childs ....s
	% ...
	ChildSpec = [],
	{ok, {{rest_for_one, 3, 30}, ChildSpec}}.
	