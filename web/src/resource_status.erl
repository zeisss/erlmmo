%% @doc Gives a global status for clients.

-module(resource_status).
-export([init/1, to_html/2, service_available/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.
    
service_available(ReqData, State) ->
    Result = case global:whereis_name(session_master) of
        undefined -> false;
        _ -> true
    end,
    {Result, ReqData, State}.
    
to_html(ReqData, State) ->
    {"<html><body>STATUS</body></html>", ReqData, State}.
