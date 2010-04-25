%% @doc Gives a global status for clients.

-module(resource_status).
-export([init/1, to_html/2, service_available/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.
    
service_available(ReqData, State) ->
    Result = not(lists:any(
        fun(ServiceName) ->
            case global:whereis_name(ServiceName) of
                undefined -> true;
                _ -> false
            end
        end, [
            % The names of the services that should be available
            session_master,
            chat_master,
            skill_master,
            zone_master
        ]
    )),
    {Result, ReqData, State}.
    
to_html(ReqData, State) ->
    {"<html><head><title>Status: OK</title></head><body>This service returns a 200 OK when all required services are available. This is currently the case.</body></html>", ReqData, State}.
