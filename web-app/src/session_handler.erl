-module(session_handler).

-compile(export_all).
-behaviour(gen_server).

-export([start_link/1, chat_say/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start_link(Username) ->
    gen_server:start_link(?MODULE, [Username], []).

chat_say(Pid, Message) ->
    io:format("~w ~w~n", [Pid, Message]),
    gen_server:cast(Pid, {chat_say, Message}).
    
    
% ------------------------------------------------------------------------------
%% Internal API

init([Username]) ->
    {ok, {Username, []}}.
    
handle_cast({chat_say, Message}, {Username, Messages} = State) ->
    io:format("~s: ~s~n", [Username, Message]),
    {noreply, State}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, ok, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
