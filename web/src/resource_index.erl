%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(resource_index).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, undefined}.
    
to_html(ReqData, State) ->
    {"<html><body>API Interface</body></html>", ReqData, State}.
