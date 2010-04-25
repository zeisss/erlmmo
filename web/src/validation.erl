-module(validation).

% This module provides simple valid_* functions to check if certain user inputs are valid, e.g. a valid_player_name()
% function for checking the playernames.
-export([
    validate_player_name/1,
    validate_player_password/1,
    
    validate_chat_message/1,
    is_system_channel_name/1,
    validate_chat_channel/1,
    validate_next_coords/2,
    
    test/0
]).

validate_player_name("") -> false;
validate_player_name(_) -> true.

validate_player_password("") -> false;
validate_player_password(_) -> true.

validate_chat_message(Msg) when is_binary(Msg) ->
    validate_chat_message(binary_to_list(Msg));
validate_chat_message("") -> false;
validate_chat_message(_) -> true.




is_system_channel_name(<<"Moderators">>) -> true;
is_system_channel_name(<<"Admins">>) -> true;
is_system_channel_name(<<"Local">>) -> true;
is_system_channel_name(_) -> false.

validate_chat_channel(Name) when is_binary(Name) ->
    validate_chat_channel(binary_to_list(Name));

validate_chat_channel("") -> false;
validate_chat_channel(Name) when length(Name) >= 15->
    false;
validate_chat_channel(Name) ->
    true.
    
    
% if the coords are no real move
validate_next_coords({X,Y}, {X,Y}) -> true;
validate_next_coords({NextX, NextY}, {CurrentX, CurrentY}) ->
    abs(CurrentX - NextX) < 2 andalso abs(CurrentY - NextY) < 2.
    
    
test() ->
    true = validate_next_coords({0,0}, {0,0}),
    true = validate_next_coords({0,0}, {1,0}),
    true = validate_next_coords({0,0}, {0,1}),
    true = validate_next_coords({0,0}, {1,1}),
    true = validate_next_coords({0,0}, {-1,0}),
    true = validate_next_coords({0,0}, {0,-1}),
    true = validate_next_coords({0,0}, {-1,-1}),
    true = validate_next_coords({0,0}, {-1,1}),
    true = validate_next_coords({0,0}, {1,-1}),
    false = validate_next_coords({-2,0}, {2,0}),
    false = validate_next_coords({0,-3}, {0,0}),
    
    
    
    
    ok.
    