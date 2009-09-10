-module(session_handler).

-compile(export_all).
-behaviour(gen_server).

-define(SESSION_TIMEOUT, 120*1000).

-export([start_link/2, chat_say/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-record(session_state, {
    username,
    sessionkey,
    timeout,
    player_object,
    messages = []
}).

% ------------------------------------------------------------------------------

start_link(Username, SessionKey) ->
    Timeout = ?SESSION_TIMEOUT,
    {ok, Pid} = gen_server:start_link(?MODULE, [Username, SessionKey, Timeout], []),
    {ok, Pid, Timeout}. 

chat_say(Pid, Message) ->
    gen_server:cast(Pid, {chat_say, Message}).
    
event_get(Pid) ->
    gen_server:call(Pid, {event_get}).
    
    
logoff(Pid) ->
    gen_server:call(Pid, {logoff}).

% ------------------------------------------------------------------------------
%% Internal API
%%
%% TODO: Upon starting, connect to the zone, we want to join
%% TODO: Send chat messages to the zone
%%
init([Username, SessionKey, Timeout]) ->
    Player = player_repository:fetch(Username),
    
    State = #session_state{
        username = Username,
        sessionkey = SessionKey,
        timeout = Timeout,
        messages = [
            [chat_message, list_to_binary("System"), list_to_binary("Welcome!")]
        ],
        player_object = Player
    },
    {ok, State, Timeout}.
  
handle_cast({chat_say, Message}, State) ->
    io:format("~s: ~s~n", [State#session_state.username, Message]),
    
    Messages = State#session_state.messages ++ [[chat_message, list_to_binary(State#session_state.username), list_to_binary(Message)]],
    
    NewState = State#session_state{messages=Messages},
    
    {noreply, NewState}.
    
  
handle_call({logoff}, _From, State) ->
    logoff_session(State),
    {stop, normal, ok, State};
    
handle_call({event_get}, _From, State) ->
    Messages = State#session_state.messages,
    NewState = State#session_state{messages=[]},
    {reply, Messages, NewState};
    
handle_call(_Request, _From, State) ->
    {noreply, State}.
    
handle_info(timeout, State) ->
    io:format("Timeout for user ~s~n", [State#session_state.username]),
    logoff_session(State),
    {stop, normal, State};
    
handle_info(_Info, State) ->
    {noreply, ok, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
logoff_session(State) ->
    session_service:logoff(State#session_state.sessionkey),
    player_repository:store(State#session_state.player_object),
    ok.
