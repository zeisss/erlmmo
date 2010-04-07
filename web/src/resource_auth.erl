%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(resource_auth).
-export([init/1, malformed_request/2, service_available/2, allowed_methods/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(state, {action, parameters}).

init([]) ->
    {ok, #state{} }.
    
% Make sure that the login is available
service_available(ReqData, State) ->
    Status = case global:whereis_name(session_master) of
        undefined -> false;
        _ -> true
    end,
    {Status, ReqData, State}.
   

% We only allow POST for the actions.
allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.
    

% Check that the given 3. url parameter (e.g. /v1/auth/$action) is either login or logoff
malformed_request(ReqData, State) ->
    case wrq:path_tokens(ReqData) of
        [] -> {true, ReqData, State};
        [Action] ->
            LoginName =     wrq:get_qs_value("login_name", ReqData),
            LoginPassword = wrq:get_qs_value("login_password", ReqData),
            ApiKey =        wrq:get_qs_value("apikey", ReqData),
            NewState =      State#state{action=Action},
            
            case Action of
                "login" ->
                    case [LoginName, LoginPassword] of
                        [undefined, _] -> {true, ReqData, NewState};
                        [_, undefined] -> {true, ReqData, NewState};
                        [_,_] -> {false, ReqData, NewState#state{parameters={LoginName, LoginPassword}}}
                    end;
                "logout" ->
                    case ApiKey of
                        undefined -> {true, ReqData, NewState};
                        _ -> {false, ReqData, NewState#state{parameters={ApiKey}}}
                    end
            end;
        _ -> {true, ReqData, State}
    end.
    
% Lets do the real work
% get the parameters
% and process them through the session_master
process_post(ReqData, State = #state{action=Action, parameters=Parameters}) ->
    {Success, Content} = case Action of
        "login" ->
            {LoginName, LoginPassword} = Parameters,
    
            {ok, ApiKey} = session_master:login(LoginName, LoginPassword),
            
            {true, {struct, [{type, ok}, {apikey, ApiKey}]}};
    
        "logout" ->
            ApiKey = wrq:get_qs_value("apikey", ReqData),
            
            case session_master:logout(ApiKey) of
                ok -> {true, <<"OK">>};
                no_session -> {true, {struct, [{code, 001}, {message, <<"Invalid sessionkey.">>}]}};
                _ ->  {false, {struct, [{type, error},{message, <<"Logout failed">>}]}}
            end
    end,
    NewReqData = wrq:set_resp_body(mochijson2:encode(Content), ReqData),
    
    {Success, NewReqData, State}.
