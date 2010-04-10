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
            SessionKey =    wrq:get_qs_value("apikey", ReqData),
            NewState =      State#state{action=Action},
            
            case Action of
                "login" ->
                    case [LoginName, LoginPassword] of
                        [undefined, _] -> {true, ReqData, NewState};
                        [_, undefined] -> {true, ReqData, NewState};
                        [_,_] -> {false, ReqData, NewState#state{parameters={list_to_binary(LoginName), list_to_binary(LoginPassword)}}}
                    end;
                "logout" ->
                    case SessionKey of
                        undefined ->
                            {true, ReqData, NewState};
                        _ ->
                            {false, ReqData, NewState#state{parameters={list_to_binary(SessionKey)}}}
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
    
            case session_master:login(LoginName, LoginPassword) of
                {ok, ApiKey} ->
                    {true, {struct, [{type, ok}, {apikey, ApiKey}]}};
                    
                {error, invalid_credentials} ->
                    {false, {struct, [{type, error}, {code, 005}, {message, <<"Invalid username/password.">>}]}};
                
                % Log any other error
                {error, Reason} ->
                    error_logger:error_msg("Error while authenticating user '~s': ~p", [LoginName, Reason]),
                    {false, {struct, [{type, error}, {code, 004}, {message, <<"Unknown error during login">>}]}}
            end;
    
        "logout" ->
            {SessionKey} = Parameters,
            
            case session_master:logout(SessionKey) of
                ok ->         {true, <<"OK">>};
                no_session -> {true, {struct, [{type, error}, {code, 001}, {message, <<"Invalid sessionkey.">>}]}};
                UnknownError ->
                    error_logger:error_msg("Error while logging out user '~s': ~p", [SessionKey:get_name(), UnknownError]),
                    {false, {struct, [{type, error},{code, 003}, {message, <<"Logout failed">>}]}}
            end
    end,
    NewReqData = wrq:set_resp_body(mochijson2:encode(Content), ReqData),
    
    {Success, NewReqData, State}.
