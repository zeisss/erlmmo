-module(zone).
-behaviour(gen_server).

-include("erlmmo.hrl").

-export([start_link/1]).
-export([join/2, part/1, move/2, attack/2, use/2]).

%%%
% Free like Bob Ross: In our little world ..
%
% ... we have a number of rooms arranged in a matrix of fields, just like in a chess game.
% Objects can join a room with join() and then move around to a wished location. 
%
%
% The joined process gets send a number of messages when something in the room changes:
%  {room_move, RoomPid, UserName, UserPid, Direction, NewPosition}
%  {room_join, RoomPid, UserName, UserPid, Door, NewPosition}
%  {room_part, RoomPid, UserName, UserPid, Door, OldPosition}
%  {room_say, RoomPid, UserName, UserPid, Message}
%
% start_link(Id) -> {ok, RoomPid}
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
% The current pid parts the room which results in a broadcast room_part message 
% to all joined pids. This is normally only used when the player goes offline.
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
% Sets the command of the current Pid to use the given object.
%
%
use(RoomPid, ObjectId) ->
    ok.

attack(RoomPid, ObjectId) ->
    ok.
   
raise_timer(RoomPid) ->
  
%% ===================================================================================== %%

-record(room_state, {name, objects=[], timer_ref}).

init([Id]) ->
   % TODO: Do something with the given Id for this room.
   Tref = timer:apply_interval(?TIME_INTERVAL, ?MODULE, raise_timer, [self()]),
   State = room_state{name=Id, timer_ref=Tref},
   {ok, State}.
    
