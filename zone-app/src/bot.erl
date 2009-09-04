-module(bot).
-compile(export_all).

%%%
% A simple bot for testing the zones
%
%

start() ->
	start('0_0_0_forest.yml', "Adam").
	
start(ZoneId, Name) ->
	spawn(?MODULE, init, [ZoneId, Name]).
	
	
init(ZoneId, Name) ->
	ZonePid = global:whereis_name(ZoneId),
	
	zone:join(ZonePid, {0,0} ), % Join the zone 
	
	loop(ZonePid,Name),
	
	zone:part(ZonePid).
	
loop(ZonePid,Name) when is_pid(ZonePid) ->
	receive 
		_ -> 
			ok
		after 5000 ->
			ok = random_action(ZonePid, Name)			
			
	end,
	loop(ZonePid, Name).
	
	
random_action(ZonePid, Name) ->
	case random:uniform(4) of
		1 -> zone:say(ZonePid, "Hello!");
		2 -> zone:say(ZonePid, "Hello there.");
		3 -> zone:say(ZonePid, "Dummdidumm...");
		4 -> bye
	end.