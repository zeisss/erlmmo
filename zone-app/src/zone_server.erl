-module(zone_server).
-include("erlmmo.hrl").

-behaviour(gen_server).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TIME_INTERVAL, 1000).

%% ===================================================================================== %%

-record(zone_state, {name, 
		     width,
                     height,
                     fields,		% A dict of fields. {X,Y} => zone_field record
		     joined_processes = [], 	% [Pid, Location] of all joined processes. Used for chatting
		     timer_ref}).

-record(zone_field, {x,y,
                     name,
		     description, 
		     blocked = 0, % 1 => blocked, 0 => free to walk around
		     objects = []  % Holds all players and objects on the current field
		    }).
				 	
-record(zone_player, {name, pid}).

%% ===================================================================================== %%
%
%
% 
init([Path, Id]) ->
   io:format("Zone"),
   io:format("~s starting.", [Id]),
   
   {ZoneName, Width, Height, FieldList} = zone_loader:load(Path, Id),
   
   % Reformat the given structure into our internal records
   NewFields = lists:foldl(
   	fun({X,Y, Name, Desc, Blocking}, Dict) ->
   		dict:store({X,Y}, #zone_field{x=X,y=Y,name=Name,description=Desc,blocked=Blocking}, Dict)
   	end,
   	dict:new(),
   	FieldList
   ),
   
   % Starts the timer that sends us a regular tick
   Tref = timer:apply_interval(?TIME_INTERVAL, zone, perform_tick, [self()]),
   			
   State = #zone_state{name=ZoneName, width=Width, height=Height, fields=NewFields, timer_ref=Tref},
   {ok, State}.

%% ===================================================================================== %%

handle_cast({join, Pid, Location={_X,_Y}}, State) ->
	io:format("Join~n"),
	
	% Update internal pid list
	NewPids = lists:append(State#zone_state.joined_processes, [[Pid, Location]]),
	
	% Update the fields
	Player = #zone_player{name="Walter", pid=Pid}, % TODO: Where to get the player's name?
	Field = dict:fetch(Location, State#zone_state.fields),
	NewField = Field#zone_field{objects=lists:append(Field#zone_field.objects, [Player])},
	NewFields = dict:store(Location, NewField, State#zone_state.fields),
	
	% New State
	NewState = State#zone_state{joined_processes=NewPids, fields=NewFields},
	{noreply, NewState};

%% ===================================================================================== %%

handle_cast({part, Pid}, State) ->
	io:format("Part~n"),
	
	% Update internal pid list
	[[Pid, Location]] = lists:filter(
		fun(X) -> case X of [Pid, _Location2] -> true; _ -> false end end, 
		State#zone_state.joined_processes
	),
	NewPids = lists:delete([Pid, Location], State#zone_state.joined_processes),
	
	% Update the fields
	Field = dict:fetch(Location, State#zone_state.fields),
	Player = lists:filter(
		fun(X) ->
			case X of 
			  #zone_player{pid=Pid} -> true;
			  _ -> false
			end
		end,
		Field#zone_field.objects
	),
	
	NewField = Field#zone_field{objects=lists:delete(Player, Field#zone_field.objects)},
	NewFields = dict:store(Location, NewField, State#zone_state.fields),
	
	% New State
	NewState = State#zone_state{joined_processes=NewPids, fields=NewFields},
	{noreply, NewState};

%% ===================================================================================== %%
		
handle_cast({move, Pid, Direction}, State) ->
	io:format("Move~n"),
	{noreply, State};

%% ===================================================================================== %%
	
handle_cast(perform_tick, State) ->
    io:format("Tick~n"),
	{noreply, State};
	
handle_cast({say, FromPid, Message}, State) ->
	{noreply, State};
	
handle_cast(Request, State) -> 
	{noreply, State}. 
	
%% ===================================================================================== %%    
handle_call(Request, From, State) ->
	{reply, [], State}.
	
handle_info(Info, State) ->
	{noreply, State}.
	
terminate(Reason, State) ->
	timer:cancel(State#zone_state.timer_ref),
	ok.
	
code_change(OldVsn, State, Extra) -> 
	{ok, State}.