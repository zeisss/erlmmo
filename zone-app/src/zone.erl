%%% -------------------------------------------------------------------
%%% File: zone.erl
%%% Description :%%%
%%% 
%%% Free like Bob Ross: In our little world ..
%%%
%%% ... we have a number of Zones arranged in a matrix of fields, just like in a chess game.
%%% Players can join a room with join() and tell the zone what they want to do (move..).
%%% The zone interprets these actions once a second and sets the updated mapdata to the client 
%%% using the player module.
%%%
%%% The implementation of the interface below is located in the zone_server.erl file.
%%% -------------------------------------------------------------------
-module(zone).

-compile(export_all).

%%%
% start_link(FileId) -> {ok, ZonePid}
% Path = FileId = [char()]
%
start_link() ->
    start_link("priv/zones", "0_0_0_forest.yml").
	
start_link(Path, FileId) when is_list(FileId) ->
    io:format("Starting zone ~s~n", [FileId]),
    gen_server:start_link({global, list_to_atom(FileId)}, zone_server, [Path, FileId], []).
    
%%%
% Sends the Zone a tick, so it calculates all commands.
% This is public for the external timer process. Do not use directly
% until you know what you do (e.g. testing, debugging)
%
% perform_tick(Pid) -> ok
%
perform_tick(ZonePid) -> 
	gen_server:cast(ZonePid, perform_tick).
  

