-module(session, [ApiKey, Name]).

-export([init/0, get_apikey/0, get_name/0]).
-export([add_message/1, get_messages_once/0]).
-export([chat_join/1, chat_send/2, chat_part/1]).

%% -----------------------------------------------------------------------------

init() ->
    % Shall we spawn a session process here?
    THIS:chat_join(<<"Global">>),
    THIS:chat_join(<<"Trade">>),
    ok.

%% -----------------------------------------------------------------------------

get_apikey() -> ApiKey.
get_name() -> Name.

%% -----------------------------------------------------------------------------

add_message(Message) ->
    io:format("~p << ~p~n", [THIS, Message]),
    session_master:session_add_message(THIS, Message).

get_messages_once() ->
    session_master:session_get_messages(THIS).

%% -----------------------------------------------------------------------------

chat_join(Channel) ->
    chat_master:chat_join(THIS, Channel).
    
chat_send(Channel, Message) ->
    chat_master:chat_send(THIS, Channel, Message).
    
chat_part(Channel) ->
    chat_master:chat_part(THIS, Channel).
    