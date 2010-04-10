-module(validation).

% This module provides simple valid_* functions to check if certain user inputs are valid, e.g. a valid_player_name()
% function for checking the playernames.
-export([
    validate_player_name/1,
    validate_chat_message/1,
    validate_chat_channel/1
]).

validate_player_name("") -> false;
validate_player_name(_) -> true.

validate_chat_message(Msg) when is_binary(Msg) ->
    validate_chat_message(binary_to_list(Msg));
validate_chat_message("") -> false;
validate_chat_message(_) -> true.

validate_chat_channel(Name) when is_binary(Name) ->
    validate_chat_channel(binary_to_list(Name));
validate_chat_channel("") -> false;
validate_chat_channel(Name) when length(Name) >= 15->
    false;
validate_chat_channel(_) ->
    true.
    