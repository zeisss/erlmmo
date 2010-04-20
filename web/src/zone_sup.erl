%% @doc Supervisor for the zones.

-module(zone_sup).
-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    ZoneMaster = {zone_master,
	   {zone_master, start_link, []},
	   permanent, 5000, worker, dynamic},
    
    {ok, [ListOfZoneNames]} = file:consult(filename:join(
                         [filename:dirname(code:which(?MODULE)),
                          "..", "priv", "zones", "zone.conf"])),
    
    Zones = lists:map (
        fun(Filename) ->
            {ok, Config} = file:consult(filename:join([
                filename:dirname(code:which(?MODULE)),
                "..","priv", "zones", Filename
            ])),
            
            ZoneId = proplists:get_value(id, Config),
            
            {ZoneId, {zone, start_link, [Config]}, permanent, 5000, worker, dynamic}
        end,
        ListOfZoneNames
    ),
    
    Processes = [ZoneMaster] ++ Zones,
    {ok, {{one_for_one, 10, 10}, Processes}}.
