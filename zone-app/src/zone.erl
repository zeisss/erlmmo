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
% Adds the current process to the room list. The current process must implement the zone_object behaviour.
%
% join(Pid, Location) -> ok
% Location -> {X,Y}
% TODO: Replace with zone_mob_info record
join(ZonePid, Location) ->
    gen_server:cast(ZonePid, {join, self(), Location}).
    
%%%
% The current pid parts the room. This is normally only used when the player goes offline.
% If players leave a zone by moving to the outtest coordinates, the zone handles the parting
% itself.
% 
% part(Pid) -> {ok, Info}
%
part(ZonePid) ->
    gen_server:cast(ZonePid, {part, self()}).

%%%
% Sets the command of the current Pid to move to the given location.
%
% move(Pid, Location) -> ok
% Direction = n | e | w | s | nw | ne | sw | se
%
move(ZonePid, Direction) ->
   gen_server:cast(ZonePid, {move, self(), Direction}).

%%%
% Sets the command of the current Pid to say the given Message.
% Chatting does NOT fall under the one command/100ms limit, as they
% are executed directly.
%
% say(ZonePid, Who, Message) -> ok
% Message = [char()]
%
say(ZonePid, Message) ->
  gen_server:cast(ZonePid, {say, self(), Message}).

%%%
% Sends the Zone a tick, so it calculates all commands.
% This is public for the external timer process. Do not use directly
% until you know what you do (e.g. testing, debugging)
%
% perform_tick(Pid) -> ok
%
perform_tick(ZonePid) -> 
	gen_server:cast(ZonePid, perform_tick).
  

