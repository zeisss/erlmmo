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
-module(resource_chat).
-export([init/1, malformed_request/2, service_available/2, allowed_methods/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {apikey, message, channel}).

init([]) ->
    {ok,
        #state{}
    }.
    
% Make sure that the session_master is available
service_available(ReqData, State) ->
    Status = case global:whereis_name(session_master) of
        undefined -> false;
        _ -> true
    end,
    {Status, ReqData, State}.

   
allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.
    

%%
% Checks that all parameters are given for the POST request
malformed_request(ReqData, State) ->
    ApiKey =        wrq:get_qs_value("apikey", ReqData),
    case ApiKey of
        undefined -> {true, ReqData, State};
        _ -> 
            Message = wrq:get_qs_value("message", ReqData),
            Channel = wrq:get_qs_value("channel", ReqData),
            
            case [Message, Channel] of
                [undefined,_] -> {true, ReqData, State};
                [_,undefined] -> {true, ReqData, State};
                _ ->             {false, ReqData, State#state{apikey=ApiKey,message=Message,channel=Channel}}
            end
    end.
       
% Lets do the real work
% get the parameters
% and process them through the session_master
process_post(ReqData, State = #state{apikey=ApiKey, message=Message, channel=Channel}) ->
    {ok, Session} = session_master:find(ApiKey),
    case Session:chat_send(Channel, Message) of
        ok -> {true, ReqData, State};
        _ -> {false, ReqData, State}
    end.
