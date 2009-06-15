-module(room).
-behaviour(gen_server).

-include("erlmmo.hrl").

-export([start_link/1]).
-export([join/2, part/2, move/2]).

%%%
% Free like Bob Ross: In our little world ..
%
% ... we have a number of room each representing a matrix of fields, just like in a chess game.
% Objects can join the room with join() and then move around in all 8 directions. If they reach the border,
% They get a special response telling them that the room ends here. The intention is that the 'user_server'
% now checks with the 'world_map' if we can cross that border. If yes, the 'user_server' part()'s the current
% room and join()s the next one.
%
% The joined process gets send a number of messages when something in the room changes:
%  {room_move, RoomPid, UserName, UserPid, Direction, NewPosition}
%  {room_join, RoomPid, UserName, UserPid, Door, NewPosition}
%  {room_part, RoomPid, UserName, UserPid, Door, OldPosition}
%  
%

start_link(Id) ->
    gen_server:start_link({global, Id}, ?MODULE, [Id], []).
    
%%%
% Adds the current process to the room list
%
join(RoomPid, Door) ->
    {ok, #room_info{name="test", description="foobar", images=["test.jpg"]} }.
    
part(RoomPid, Door) ->
    ok.

move(RoomPid, Direction) ->
   ok.

init([Id]) ->
   % TODO: Do something with the given Id for this room.
   {ok, []}.
    
