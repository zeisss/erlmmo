%%% -------------------------------------------------------------------
%%% File: zone.erl
%%% Description :%%%
%%% 
%%% Free like Bob Ross: In our little world ..
%%%
%%% ... we have a number of Zones arranged in a matrix of fields, just like in a chess game.
%%% Objects can join a room with join() and tell the zone what they want to do (move, use..).
%%% The zone interprets these actions once a second and sets the updated mapdata to the client 
%%% using the player module.
%%%
%%% The implementation of the interface below is located in the zone_server.erl file.
%%% -------------------------------------------------------------------
-module(zone).

-compile(export_all).

%%%
% start_link(Id) -> {ok, RoomPid}
%
start_link(Id) ->
    gen_server:start_link({global, Id}, ?MODULE, [Id], []).
    
%%%
% Adds the current process to the room list which results in a broadcast room_join message
% to all joined pids.
%
% join(Pid, Direction) -> ok
% Direction -> atom() | {X,Y}
%
join(RoomPid, Direction) ->
    ok.
    
%%%
% The current pid parts the room. This is normally only used when the player goes offline.
% If players leave a zone by moving to the outtest coordinates, the zone handles the parting
% itself.
% 
% part(Pid) -> {ok, Info}
%
part(RoomPid) ->
    ok.

%%%
% Sets the command of the current Pid to move to the given location.
%
% move(Pid, Location) -> ok
% Location = {X,Y}
%
move(RoomPid, Location) ->
   ok.

%%%
% Sets the command of the current Pid to say the given Message.
% Chatting does NOT fall under the once command/100ms limit, as they
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
  

