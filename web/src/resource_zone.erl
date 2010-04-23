%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

%%
% This resource provides the following urls:
% POST /v1/{sessionkey}/zone    path=[[x,y],[x1,y1]]    Sets a path along the given list
%%
-module(resource_zone).
-export([init/1, malformed_request/2, resource_exists/2, service_available/2, allowed_methods/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {sessionkey, session, path}).

init([]) ->
    {ok,
        #state{}
    }.
    
% Make sure that the session_master is available
service_available(ReqData, State) ->
    Status = case global:whereis_name(zone_master) of
        undefined -> false;
        _ -> true
    end,
    {Status, ReqData, State}.

   
allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.
    

%%
% Checks that all parameters are given for the POST request
malformed_request(ReqData, State) ->
    SessionKey = case wrq:path_info(sessionkey, ReqData) of
        undefined -> wrq:get_qs_value("apikey", ReqData);
        Data -> mochiweb_util:unquote(Data)
    end,
    
    case SessionKey of
        undefined -> {true, ReqData, State};
        _ ->
            Path = wrq:req_body(ReqData),
            
            case Path of
                'undefined' -> {true, ReqData, State};
                JsonData ->
                    List = mochijson2:decode(JsonData),
                    
                    {not(lists:all(
                        fun(Ele) ->
                            case Ele of
                                [B,C] when is_integer(C), is_integer(B) -> true;
                                _ -> false
                            end
                        end,
                        List
                     )), ReqData, State#state{path=lists:map(fun([X,Y]) -> {X,Y} end, List), sessionkey=SessionKey}}
            end
    end.
       
resource_exists(ReqData, State = #state{sessionkey=Sessionkey}) ->
    case session_master:find(Sessionkey) of
        {error, no_session} -> {false, ReqData, State};
        {ok, Session} ->
            {true, ReqData, State#state{session=Session}}
    end.
    


% Lets do the real work
% get the parameters
% and process them through the session_master
process_post(ReqData, State = #state{session=Session, path=Path}) ->
    zone_master:zone_set_course(Session, Path),
    NewReqData = wrq:set_resp_body(mochijson2:encode(<<"OK">>), ReqData),
    {true, NewReqData, State}.
