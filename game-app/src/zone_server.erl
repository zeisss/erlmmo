-module(zone_server).

-include("erlmmo.hrl").
-behaviour(gen_server).

-define(TIME_INTERVAL, 1000).

%% ===================================================================================== %%

-record(zone_state, {name, objects=[], timer_ref}).


%
% 
init([Id]) ->
   % TODO: Do something with the given Id for this room.
   
   % Starts the timer that sends us a regular tick
   Tref = timer:apply_interval(?TIME_INTERVAL, ?MODULE, raise_timer, [self()]);
   			
   State = zone_state{name=Id, timer_ref=Tref},
   {ok, State}.
    
    
handle_call(Request, {Pid,_Tag}, State) ->
	{reply, [], State}.
	
handle_cast(perform_tick, State) ->
    io:format("Tick~n"),
	{noreply, State};
	
handle_cast({say, FromPid, Message}, State) ->
	{noreply, State};
	
handle_cast(Request, State) -> 
	{noreply, State}. 
	
	
handle_info(Info, State) ->
	{noreply, State}.
	
	
terminate(Reason, State) ->
	ok.
	
code_change(OldVsn, State, Extra) -> 
	{ok, NewState}.