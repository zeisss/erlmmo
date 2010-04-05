-module(session, [ApiKey, Name]).

-export([init/0, get_apikey/0, get_name/0]).
-export([add_message/1, get_messages/0]).
-export([chat_join/1, chat_send/2, chat_part/1]).

%% -----------------------------------------------------------------------------

init() ->
    % Shall we spawn a session process here?
    ok.

%% -----------------------------------------------------------------------------

get_apikey() -> ApiKey.
get_name() -> Name.

%% -----------------------------------------------------------------------------

add_message(Message) ->
    io:format("~w~n", [Message]).

get_messages() ->
    [].

%% -----------------------------------------------------------------------------

chat_join(Channel) ->
    ok.
    
chat_send(Channel, Message) ->
    ok.
    
chat_part(Channel) ->
    ok.
    