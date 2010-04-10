%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

%%
% This resource provides the following urls:
% GET /v1/chat      Returns all pending chat messages for the session  
% POST /v1/chat     message=MyTextBlaBlub&range=local      Post a message to the other sessions
%
% local: Just the current fields
% system: The whole system
% sub: Subrange (All systems)
% global: Important Message on all Systems (Admin only)
%%
-module(resource_event).
-export([init/1, service_available/2, allowed_methods/2, resource_exists/2, content_types_provided/2, to_javascript/2]).

-export([malformed_request/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {sessionkey, session}).

init([]) ->
    {ok,
        #state{}
    }.

%%    
% Make sure that the session_master is available
service_available(ReqData, State) ->
    Status = case global:whereis_name(chat_master) of
        undefined -> false;
        _ -> true
    end,
    {Status, ReqData, State}.

%%
% Only allow 'POST's
allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.
    
%%
% Checks that all parameters are given for the POST request
malformed_request(ReqData, State) ->
    SessionKey =        wrq:get_qs_value("apikey", ReqData),
    case SessionKey of
        undefined ->    {true,  ReqData, State};
        _ ->            {false, ReqData, State#state{sessionkey=SessionKey}}
    end.
       
resource_exists(ReqData, State = #state{sessionkey=SK}) ->
    case session_master:find(SK) of
        {error, no_session} -> {false, ReqData, State};
        {ok, Session} ->
            {true, ReqData, State#state{session=Session}}
    end.
    
content_types_provided(ReqData, State) ->
    {
        [
            {"text/javascript", to_javascript}
        ],
        ReqData, State
    }.
    
%%
% 
to_javascript(ReqData, State) ->
    {"true", ReqData, State}.

process_post(ReqData, State = #state{session=Session}) ->
    {ok, Events} = Session:get_messages_once(),
    Content = transform_messages(Events),
    NewReqData = wrq:set_resp_body(mochijson2:encode(Content), ReqData),
    
    {true, NewReqData, State}.

    
%% Transforms all events to a mochijson compatible format
transform_messages(Events) ->
    lists:map(fun(X) -> transform_message(X) end, Events).
    
transform_message({chat_join_self, ChannelName, Players}) ->
    {struct, [{type, chat_join_self},
              {name, ChannelName},
              {players, lists:map(fun(X) -> transform_player(X) end, Players)}]};
transform_message({chat_join, ChannelName, PlayerName}) ->
    {struct, [{type, chat_join},
              {name, ChannelName},
              {player, transform_player(PlayerName)}]};
transform_message({chat_part_self, ChannelName}) ->
    {struct, [{type, chat_part_self},
              {name, ChannelName}]};
transform_message({chat_send, ChannelName, PlayerName, Message}) ->
    {struct, [{type, chat_send},
              {name, ChannelName},
              {player, PlayerName},
              {message, Message}]};
transform_message({chat_part, ChannelName, PlayerName}) ->
    {struct,  [{type, chat_part},
               {name, ChannelName},
               {player, PlayerName}]};
transform_message(OtherEvent) ->
    OtherEvent.
    

transform_player(Player) when is_list(Player) ->
    list_to_binary(Player);
transform_player(Player) when is_binary(Player)->
    Player.