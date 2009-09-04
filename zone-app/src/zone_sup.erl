%%%-------------------------------------------------------------------
%%% File    : zone_sup.erl
%%% Author  : Stephan Zeissler
%%% Description :
%%%  This module is a supervisor starting a number of child supervisors.
%%%
%%% Children:
%%%  ??
%%%
%%%-------------------------------------------------------------------
-module(zone_sup).

-behaviour (supervisor).

%% API
-export([start_link/0, start_link/1, stop/0]).

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
	start_link({zone_files, "priv/zones/"}).
start_link({zone_files, Path}) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Path]).
	
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
    
%%=========================================================================
%% Supervisor callbacks
%%=========================================================================
%%-------------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%-------------------------------------------------------------------------
init([Path]) ->
	io:format("zone_sup starting with zones from ~s:~n", [Path]),
	{ok, Filenames} = file:list_dir(Path),
	
	ChildSpec = lists:map(
		fun(File) ->
			io:format(" ~s~n", [File]),
			{zone, {zone, start_link, [Path, File]}, permanent, 10, worker, [gen_server]}
		end,
		lists:filter(fun(X) -> case X of "README" -> false; _ -> true end end, Filenames)
	),	
	%ChildSpec = [
	%   {zone0_0_0, {zone, start_link, ["priv/zones", "0_0_0_forest.yml"]}, permanent, 10, worker, [gen_server]}
	%],
	{ok, {{one_for_one, 3, 30}, ChildSpec}}.
	