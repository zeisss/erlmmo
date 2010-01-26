-module(zone_server).
-include("erlmmo.hrl").

-behaviour(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TIME_INTERVAL, 10000). % TODO: set to 100

%% ===================================================================================== %%

-record(zone_state, {name, 
		     timer_ref,
                     t_field,
                     t_location,
                     t_player
                    }).

-record(zone_field, {location,
                     name,
		     description, 
		     blocked = 0 % 1 => blocked, 0 => free to walk around
		    }).
				 	
-record(zone_player, {pid, name, icon, location}).

%% ===================================================================================== %%
%
%
% 
init([Path, Id]) ->
   io:format("Zone ~s starting.~n", [Id]),
   
   {ZoneName, _Width, _Height, FieldList} = zone_loader:load(Path, Id),
   
   FieldTable = ets:new(t_field_info, [set, private, {keypos,2}]),
   LocationTable = ets:new(t_player_location, [set, private]),
   PlayerTable = ets:new(t_player, [set, private, {keypos,2}]),
   
   % Reformat the given structure into our internal records
   lists:foreach(
   	fun({X,Y, Name, Desc, Blocking}) ->
   	    ets:insert(
                FieldTable,
                #zone_field{
                    location={X,Y},
                    name=Name,
                    description=Desc,
                    blocked=Blocking
                }
            )
   	end,
   	FieldList
   ),
   
   % Starts the timer that sends us a regular tick
   Tref = timer:apply_interval(?TIME_INTERVAL, zone, perform_tick, [self()]),
   			
   State = #zone_state{name=ZoneName, timer_ref=Tref, t_field=FieldTable, t_location=LocationTable, t_player=PlayerTable},
   {ok, State}.

%% ===================================================================================== %%
	
handle_cast(perform_tick, State) ->
    
    % Print Status info
    PlayerInfos = ets:info(State#zone_state.t_player),
    FieldInfos = ets:info(State#zone_state.t_field),
    
    io:format("Tick Players: ~w Fields: ~w ~n", [proplists:get_value(size, PlayerInfos), proplists:get_value(size, FieldInfos)]),
    
    % Perform Actions ...
    
    
    % Send map state to the players
    lists:foreach(
        fun(Player) ->
            {X,Y} = Player#zone_player.location,
            MapInfo = extract_map_state(X,Y, State),
            
            zone_mob_event:send_map_info(
                Player#zone_player.pid,
                MapInfo
            )
        end,        
        ets:tab2list(State#zone_state.t_player)
    ),
    
    
    {noreply, State};
	
%% ===================================================================================== %%

handle_cast(_Request, State) -> 
	{noreply, State}. 
	
%% ===================================================================================== %%    
handle_call(_Request, _From, State) ->
	{reply, [], State}.
	
%% ===================================================================================== %%

% TODO: this shouldn't be done automatically, but only on a tick
handle_info({zone_move, Pid, Direction}, State) ->
	io:format("Move~n"),
        
        [Player] = ets:lookup(State#zone_state.t_player, Pid),
        Location = {X,Y} = Player#zone_player.location,
        
        io:format("Location: ~w~n", [Location]),
        
        NewLocation = case Direction of
            "n" -> {X,Y-1};
            "ne" -> {X+1,Y-1};
            "e" -> {X+1, Y};
            "se" -> {X+1, Y+1};
            "s" -> {X, Y+1};
            "sw" -> {X-1, Y+1};
            "w" -> {X-1, Y};
            "nw" -> {X-1, Y-1}
        end,
        
        case ets:member(State#zone_state.t_field, NewLocation) of
            true ->
                ets:delete_object(State#zone_state.t_location, {Location, Pid}),
                
                OldPlayers = ets:lookup(State#zone_state.t_location, Location),
                NewPlayers = ets:lookup(State#zone_state.t_location, NewLocation),
                
                ets:insert(State#zone_state.t_location, {NewLocation, Pid}),
                ets:update_element(State#zone_state.t_player, Pid, {4, NewLocation}),
                
                % Inform the old players
                lists:foreach(
                    fun({_, PlayerPid}) ->
                        PlayerPid ! {event, {field_leave, Player#zone_player.name, Direction}}
                    end, OldPlayers                
                ),
                
                % Inform the new players
                lists:foreach(
                    fun({_, PlayerPid}) ->
                        PlayerPid ! {event, {field_join, Player#zone_player.name, Player#zone_player.icon, Direction}}
                    end, NewPlayers                
                ),
                
                % Inform the player
                % TODO: THis is the same as in zone_join -> abstract into own function?
                Info = lists:map(
                    fun({_, PlayerPid}) ->
                        [PlayerObj] = ets:lookup(State#zone_state.t_player, PlayerPid),
                        
                        [PlayerObj#zone_player.name, PlayerObj#zone_player.icon]
                    end, NewPlayers
                ),
                
                % TODO: Add the real field data here
                Pid ! {event, {field_info, NewLocation, Info}};
            false ->
                Pid ! {event, {error, no_location_there}}
        end,
    
	{noreply, State};

%% ===================================================================================== %%
        
handle_info({chat_send, Pid, Message}, State) ->
    
    [Player] = ets:lookup(State#zone_state.t_player, Pid),
    Location = {X,Y} = Player#zone_player.location,
    
    io:format(" ~s (~w,~w): ~s~n", [Player#zone_player.name, X, Y, Message]),
    
        
    Players = ets:lookup(State#zone_state.t_location, Location), % Get the players we have there
    
    lists:foreach(
        fun({_, PlayerPid}) ->
            PlayerPid ! {event, {chat_message, Player#zone_player.name, Message}}
        end, Players
    ),
    
    {noreply, State};
%% ===================================================================================== %%

handle_info({zone_join, Username, Pid, IconUrl, Location={X,Y}}, State) ->
	io:format("Joining: ~s~n", [Username]),
        
        Players = ets:lookup(State#zone_state.t_location, Location), % Get the players we have there
        
        ets:insert (State#zone_state.t_location, {Location, Pid}), % Store the location for the player
        ets:insert (State#zone_state.t_player, #zone_player{pid=Pid, name=Username, icon=IconUrl, location=Location}),
        
        lists:foreach(
            fun({_, PlayerPid}) ->
                % Notify the other players that Pid has joined at the current location
                zone_mob_event:send_zone_join(
                    PlayerPid, Username, IconUrl, Location
                )
            end,
            Players
        ),
        
        Mobs = lists:map(
            fun({_, PlayerPid}) ->
                [PlayerObj] = ets:lookup(State#zone_state.t_player, PlayerPid),
                [PlayerObj#zone_player.name, PlayerObj#zone_player.icon]
            end, Players
        ),
        zone_mob_event:send_field_info(
            Pid,
            Location,
            Mobs
        ),
        
        MapInfo = extract_map_state(X,Y, State),
        zone_mob_event:send_map_info(Pid, MapInfo),
	
	{noreply, State};

%% ===================================================================================== %%

handle_info({zone_part, Pid}, State) ->
	[Player] = ets:lookup(State#zone_state.t_player, Pid),
        
        io:format("Part ~s~n", [Player#zone_player.name]),
        
        Location = Player#zone_player.location,
        
        ets:delete_object(State#zone_state.t_player, Player),
        ets:delete_object(State#zone_state.t_location, {Location, Pid}),
        
        % Notify players on the current field
        Players = ets:lookup(State#zone_state.t_location, Location),
        lists:foreach(
            fun({_, PlayerPid}) ->
                zone_mob_event:send_zone_part(
                    PlayerPid, Player#zone_player.name, Location
                )
            end, Players
        ),
        
	{noreply, State};
        
handle_info(_Info, State) ->
	{noreply, State}.
	
%% ===================================================================================== %%

terminate(_Reason, State) ->
        ets:delete(State#zone_state.t_player),
        ets:delete(State#zone_state.t_location),
        ets:delete(State#zone_state.t_field),
	timer:cancel(State#zone_state.timer_ref),
	ok.
	
%% ===================================================================================== %%

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
        
%% ===================================================================================== %%

%% Returns an array containing the 5x5 area around the given location in an array containing the following infos:
%% extract_map_state(X,Y, State) -> [{X, Y, PlayerCount}]
extract_map_state(X,Y,  State) ->
    lists:map(
        fun({RelX,RelY}) ->
            Length = length(ets:lookup(State#zone_state.t_location, {RelX,RelY} )),
            {RelX,RelY, Length}
        end,
        lists:flatten(lists:map(
            fun(RelX) ->
                lists:map(
                    fun(RelY) ->
                        {RelX,RelY}
                    end,
                    [Y-2, Y-1, Y, Y+1, Y+2]
                )
            end,
            [X-2, X-1, X, X+1, X+2]
        ))
    ).