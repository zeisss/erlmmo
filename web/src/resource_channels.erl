%%
% This resource returns a simple JSON list of channel that the system currently knows about.
%
% This resource provides the following urls:
% GET /v1/channels      Returns all channels, that currently exist
%%
-module(resource_channels).
-export([init/1, service_available/2, allowed_methods/2, content_types_provided/2, to_javascript/2]).

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
    {['GET'], ReqData, State}.
  
    
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

    {ok, ChannelNames} = chat_master:channel_list(),
    
    % ChannelNames is a list of binary strings
    {mochijson2:encode(ChannelNames), ReqData, State}.