-module(session_handler).

-compile(export_all).
-behaviour(gen_server).
-include("records.hrl").

-define(SESSION_TIMEOUT, 120*1000).

-export([start_link/2, chat_say/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-record(session_state, {
    username,
    sessionkey,
    timeout,
    player_object,
    messages = [],
    zone_pid
}).

% ------------------------------------------------------------------------------

start_link(Username, SessionKey) ->
    Timeout = ?SESSION_TIMEOUT,
    case gen_server:start_link(?MODULE, [Username, SessionKey, Timeout], []) of
        {ok, Pid} ->
            {ok, Pid, Timeout};
        {error, Reason} ->
            {error, Reason}
    end.
        

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
    {ok, Player} = player_repository:fetch(Username),
    io:format("a~n"),
    
    State = #session_state{
        username = Username,
        sessionkey = SessionKey,
        timeout = Timeout,
        messages = [
            [chat_message, list_to_binary("System"), list_to_binary("Welcome!")]
        ],
        player_object = Player,
        zone_pid = global:whereis_name(Player#player.zone)
    },
    io:format("b~n"),
    case State#session_state.zone_pid of
        undefined ->
            {stop, zone_not_running};
        Pid ->
            Pid ! {zone_join, Username, self(), "TODO", {0,0}},
            io:format("c~n"),
            {ok, State, Timeout}
    end.
  
handle_cast({chat_say, Message}, State) ->
    io:format("~s: ~s~n", [State#session_state.username, Message]),
    
    %Messages = State#session_state.messages ++ [[chat_message, list_to_binary(State#session_state.username), list_to_binary(Message)]],
    State#session_state.zone_pid ! {
        chat_send,
        self(),
        Message
    },
    
    %NewState = State#session_state{messages=Messages},
    % {noreply, NewState}.
    {noreply, State}.
    
  
handle_call({logoff}, _From, State) ->
    logoff_session(State),
    {stop, normal, ok, State};
    
handle_call({event_get}, _From, State) ->
    Messages = State#session_state.messages,
    NewState = State#session_state{messages=[]},
    {reply, Messages, NewState};
    
handle_call(_Request, _From, State) ->
    {noreply, State}.
    
handle_info({event, Event}, State) ->
    NewEvent = case Event of
        {field_info, {X,Y}, Players} ->
            [
             field_info,
             [X,Y],
             lists:map(fun([Username, IconUrl]) ->
                [list_to_binary(Username), list_to_binary(IconUrl)]
             end, Players)
            ];
        {field_join, Username, IconUrl, Direction} -> [field_join, list_to_binary(Username), list_to_binary(IconUrl), Direction];
        {field_leave, Username, Direction} -> [field_leave, list_to_binary(Username), Direction];
        
        {map_info, MapInfo} -> [map_info, lists:map(  fun({RelX,RelY, PlayerCount}) -> [RelX,RelY,PlayerCount] end, MapInfo)];
        
        {zone_join, Username, IconUrl, {X,Y}} -> [zone_join, list_to_binary(Username), list_to_binary(IconUrl), [X,Y]];
        {zone_part, Username, {X,Y}} -> [zone_join, list_to_binary(Username), [X,Y]];
        
        {chat_message, Username, Message} -> [chat_message, list_to_binary(Username), list_to_binary(Message)]
    end,
    
    NewMessages = State#session_state.messages ++ [NewEvent],
    NewState = State#session_state{messages=NewMessages},
    {noreply, NewState};
    
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
    State#session_state.zone_pid ! {zone_part, self()},
    
    session_service:logoff(State#session_state.sessionkey),
    player_repository:store(State#session_state.player_object),
    ok.
