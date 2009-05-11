-module(world_map).
-behaviour(gen_server).
%%%
% This process acts as a question service how different rooms are connected. Rooms are currently
% arranged in a simple chess-like style, but this can be modified by any means.
%
%%%
-define(SERVER, {global, ?MODULE}).

-export([can_move_to_direction/2]).

-export([start_link/0, init/1, handle_call/3]).

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

    
%%%
% Checks whether there is room in given direction relative to the given Room.
%
% can_move_to_direction(RoomId, Direction) -> {ok, NewRoomId} | false | {error, Reason}
% RoomId = pid()
% Direction = atom() e.g. n | s | e | w
%%%
can_move_to_direction(RoomId, Direction) ->
    gen_server:call(?SERVER, {can_move_to_direction, RoomId, Direction}).
    
    
%% ----------------------------------------------------------------------------------------    
init([]) ->
    {ok, []}.
    
handle_call({can_move_to_direction, _RoomId, _Direction}, _From, State) ->
    % TODO: Perform a real check in the database whethere there is a door to another room.
    {reply, false, State}.