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
-export([init/1, service_available/2, allowed_methods/2, malformed_request/2, content_types_provided/2, to_javascript/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {apikey}).

init([]) ->
    {ok,
        #state{}
    }.

%%    
% Make sure that the session_master is available
service_available(ReqData, State) ->
    Status = case global:whereis_name(session_master) of
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
    ApiKey =        wrq:get_qs_value("apikey", ReqData),
    case ApiKey of
        undefined ->    {true,  ReqData, State};
        _ ->            {false, ReqData, State#state{apikey=ApiKey}}
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
to_javascript(ReqData, State = #state{apikey=ApiKey}) ->
    {ok, Session} = session_master:find(ApiKey),
    
    Events = Session:get_messages_once(),
    
    Content = mochijson2:encode(Events),
    
    {Content, ReqData, State}.
