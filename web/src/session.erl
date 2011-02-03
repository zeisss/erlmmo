-module(session, [ApiKey, Name]).

-include_lib("include/erlmmo.hrl").

-export([init/0, logout/0, get_apikey/0, get_name/0]).
-export([add_message/1, get_messages_once/0]).
-export([chat_join/1, chat_send/2, chat_part/1]).

%% -----------------------------------------------------------------------------

init() ->
    TYPE = THIS,
    chat:connect(THIS, [
        % {callback, fun(Msg) -> TYPE:add_message(Msg) end}
    ]),
    
    % Shall we spawn a session process here?
    ChannelList = case storage:load_object({session, THIS, channels}) of
        {ok, undefined} -> [<<"Global">>, <<"Trade">>];
        {ok, Channels} -> Channels
    end,
    
    lists:foreach(
        fun(ChannelName) ->
            chat:join(THIS, ChannelName, [])
        end,
        ChannelList
    ),
    
    %%
    % Special Admin stuff
    case storage:load_object({session, THIS, account}) of
        % ooooopps, thats not good.
        {ok, undefined} ->
            error_logger:error_msg("[SESSION] Uhm, I can't find my 'account' informations: ~p~n", [THIS]),
            ok;
        {ok, Account} ->
            Level = Account#account_info.admin_level,
            case Level of
                5 ->
                    chat_master:chat_join(THIS, channel_moderators);
                10 ->
                    chat_master:chat_join(THIS, channel_moderators),
                    chat_master:chat_join(THIS, channel_admins);
                _ ->
                    ok
            end
    end,
            
    zone_master:zone_join(THIS, zone_1, {0,0}),
    
    ok.

logout() ->
    % Bye bye zone
    zone_master:kill_session(THIS),
    
    % Remove the session from all channels
    {ok, ChannelList} = chat_master:chat_list(THIS),
    storage:save_object({session, THIS, channels}, ChannelList),
    
    chat:disconnect(THIS, [{reason, <<"quit">>}]),
    
    % End skill training
    skill_master:session_logout(THIS).
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
    chat:chat(THIS, Channel, []).
    
chat_send(Channel, Message) ->
    chat:send(THIS, Channel, Message).
    
chat_part(Channel) ->
    chat:part(THIS, Channel, []).
    