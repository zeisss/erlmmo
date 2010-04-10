%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

%%
% Provides the possibility for the user to join/part channels.
%
% Example URLS:
% /v1/chat/<CHANNELNAME>?apikey=asdasfd
% /v1/<SESSIONKEY>/chat/<CHANNELNAME>
% /v1/<SESSIONKEY>/chat/<CHANNELNAME>?action=delete
%%
-module(resource_chat_channels).
-export([init/1, malformed_request/2, resource_exists/2, service_available/2, allowed_methods/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {sessionkey, session, channel, action}).

init([]) ->
    {ok,
        #state{}
    }.
    
% Make sure that the session_master is available
service_available(ReqData, State) ->
    Status = case global:whereis_name(chat_master) of
        undefined -> false;
        _ -> true
    end,
    {Status, ReqData, State}.

   
allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.
    

%%
% Checks that all parameters are given for the POST request
malformed_request(ReqData, State) ->
    % Support alternate ways to pass the sessionkey
    SessionKey = case wrq:path_info(sessionkey, ReqData) of
        undefined -> wrq:get_qs_value("apikey", ReqData);
        A -> mochiweb_util:unquote(A)
    end,
    Channel = wrq:path_info(channel, ReqData),
    Action = case wrq:get_qs_value("action", ReqData) of
        undefined -> "join";
        "join" -> "join";
        "part" -> "part";
        _ -> undefined
    end,
    
    case [SessionKey, Action, Channel] of
        [undefined,_,_] -> {true, ReqData, State};
        [_,undefined,_] -> {true, ReqData, State};
        [_,_,undefined] -> {true, ReqData, State};
        _ ->
            {false, ReqData, State#state{sessionkey=list_to_binary(SessionKey),channel=list_to_binary(Channel), action=Action}}
    end.
       
resource_exists(ReqData, State = #state{sessionkey=Sessionkey}) ->
    io:format("~p~n", [State]),
    
    case session_master:find(Sessionkey) of
        {error, no_session} ->
            {false, ReqData, State};
        {ok, Session} ->
            {true, ReqData, State#state{session=Session}}
    end.
    
process_post(ReqData, State = #state{session=Session, channel=Channel,action=Action}) ->
    
    case Action of
        "join" ->
            ok = Session:chat_join(Channel);
        "part" ->
            ok = Session:chat_part(Channel)
    end,
    
    NewReqData = wrq:set_resp_body(mochijson2:encode(<<"OK">>), ReqData),
    {true, NewReqData, State}.
