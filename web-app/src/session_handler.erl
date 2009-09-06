-module(session_handler).

-compile(export_all).
-behaviour(gen_server).

-define(SESSION_TIMEOUT, 120*1000).

-export([start_link/2, chat_say/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start_link(Username, SessionKey) ->
    Timeout = ?SESSION_TIMEOUT,
    {ok, Pid} = gen_server:start_link(?MODULE, [Username, SessionKey, Timeout], []),
    {ok, Pid, Timeout}. 

chat_say(Pid, Message) ->
    gen_server:cast(Pid, {chat_say, Message}).
    
    
% ------------------------------------------------------------------------------
%% Internal API
%%
%% TODO: Upon starting, conncet to the zone, we want to join
%% TODO: Send chat messages to the zone
%%
init([Username, SessionKey, Timeout]) ->
    {ok, {Username, SessionKey, []}, Timeout}.
    
handle_cast({chat_say, Message}, {Username, SessionKey, Messages} = State) ->
    io:format("~s: ~s~n", [Username, Message]),
    {noreply, State}.
    
handle_call(_Request, _From, State) ->
    {noreply, State}.
    
handle_info(timeout, State = {Username, SessionKey, Messages}) ->
    io:format("Timeout for user ~s~n", [Username]),
    
    session_service:logoff(SessionKey),
    
    {stop, normal, State};
    
handle_info(_Info, State) ->
    {noreply, ok, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
