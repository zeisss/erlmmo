-module (session_master).

-export([start_link/0, login/2, logout/1, find/1, logout_all/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([session_add_message/2, session_get_messages/1]).

-record(state, {session_counter=0, table, session_table, timeout_table}).


-define(SERVER, {global, ?MODULE}).
-define(TABLE, sessions).
-define(TIMEOUT, 5 * 60 * 1000). % Timeout, before checking for timeouts

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

%%
% Checks the credentials for the given player and starts a new session.
% The sessionid for the new session is returned.
% If the user was previously logged in, he will be logged out.
%
% login(string(), string()) -> {ok, ApiKey} |�{error, ErrorMessage}
% 
login(Name, Password) ->
    gen_server:call(?SERVER, {login, Name, Password}).

%
% Logs the given session out.
%
% logout(ApiKey) -> ok |�no_session
logout(ApiKey) when is_list(ApiKey) ->
    logout(list_to_binary(ApiKey));
logout(ApiKey) when is_binary(ApiKey) ->
    gen_server:call(?SERVER, {logout, ApiKey}).
    
    
logout_all() ->
    gen_server:call(?SERVER, logout_all).
    
%
% get(apikey()) -> {ok, Session}
% Session:

find(ApiKey) when is_list(ApiKey) ->
    find(list_to_binary(ApiKey));
find(ApiKey) when is_binary(ApiKey) ->
    case gen_server:call(?SERVER, {find, ApiKey}) of
        no_session -> {error, no_session};
        Session -> {ok, Session}
    end.

%%
% INTERNAL API for session module.
session_add_message(Session, Message) ->
    gen_server:call(?SERVER, {session_add_message,Session, Message}).
    
%%
% INTERNAL API for session module.
session_get_messages(Session) ->
    gen_server:call(?SERVER, {session_get_messages, Session}).

% ------------------------------------------------------------------------------

init([]) ->
    Tid = ets:new(?TABLE, [set, protected, {keypos,2}]),
    TimeoutTid = ets:new(timeout_table, [set, protected]),
    SessionTid = ets:new(session_messages, [set, protected]),
    
    {ok, #state{table=Tid, timeout_table=TimeoutTid, session_table=SessionTid}}.
    
handle_call({login, Name, Password}, From, State = #state{table=Tid, timeout_table=TimeoutTid}) ->
    case ets:match_object(Tid, {session, '_', Name}) of
        [Session] -> session_logout(Session, State);
        [] -> ok
    end, 
    
    Counter = State#state.session_counter,
    
    % TODO: Create a real random apikey here
    ApiKey = erlang:list_to_binary([<<"TODO:APIKEY">>, erlang:list_to_binary(erlang:integer_to_list(Counter))]),
    
    NewSession = session:new(ApiKey, Name),
    case NewSession:init() of
        ok ->
            ets:insert(Tid, NewSession),
            ets:insert(TimeoutTid, {NewSession, erlang:now()}),
            
            {reply, {ok, ApiKey}, State#state{session_counter=Counter+1}, ?TIMEOUT};
        _Error ->
            {reply, {error, failed_while_init_session}, State, ?TIMEOUT}
    end;

handle_call(logout_all, _From, State = #state{table=Tid}) ->
    lists:foreach(
        fun(X) ->
            session_logout(X, State)
        end,
        ets:tab2list(Tid)
    ),
    {reply, ok, State};
    
handle_call({logout, ApiKey}, _From, State = #state{table=Tid}) ->
    case ets:lookup(Tid, ApiKey) of
        [Session] -> {reply, session_logout(Session, State), State, ?TIMEOUT};
        [] -> {reply, no_session, State, ?TIMEOUT};
        Result -> {stop, {multiple_sessions_for_apikey, ApiKey, Result}, no_session, State}
    end;
    
handle_call({find, ApiKey}, _From, State = #state{table=Tid, timeout_table=TimeoutTid}) ->
    case ets:lookup(Tid, ApiKey) of
        [Session] ->
            ets:insert(TimeoutTid, {Session, erlang:now()}),
            {reply, Session, State, ?TIMEOUT};
        [] -> {reply, no_session, State, ?TIMEOUT};
        Result -> {stop, {multiple_sessions_for_apikey, ApiKey, Result}, no_session, State}
    end;
    
    
handle_call({session_add_message, Session, Message}, _From, State = #state{session_table=Tid}) ->
    Messages = case ets:lookup(Tid, Session) of
        [] -> [];
        [{Session, Msgs}] -> Msgs;
        _ -> []
    end,
    
    ets:insert(Tid, {Session, Messages ++ [Message]}),
    {reply, ok, State, ?TIMEOUT};
    
handle_call({session_get_messages, Session}, _From, State = #state{session_table=Tid}) ->
    Messages = case ets:lookup(Tid, Session) of
        [] -> [];
        [{Session, Msgs}] -> Msgs
    end,
    true = ets:delete(Tid, Session),
    
    {reply, {ok, Messages}, State, ?TIMEOUT}.    
    
handle_cast(_Message, State) ->
    {noreply, State, ?TIMEOUT}.
    
handle_info(timeout, State) ->
    io:format("Performing session timeout cleanup~n"),
    case internal_kill_timeouts(State) of
        ok ->
            io:format(" done (timeout)~n"),
            {noreply, State, ?TIMEOUT};
        Error ->
            {stop, {error, while_performing_cleanup, Error}, State}
    end.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
internal_kill_timeouts(State = #state{timeout_table=Tid}) ->
    ets:safe_fixtable(Tid, true),
    Result = (catch internal_kill_timeouts(State, erlang:now(), ets:first(Tid))),
    ets:safe_fixtable(Tid, false),
    Result.

internal_kill_timeouts(_, _, '$end_of_table') -> ok;
internal_kill_timeouts(State = #state{timeout_table=Tid}, Now, Session) ->
    [{Session, Timestamp}] = ets:lookup(Tid, Session),
    io:format("Checking ~p~n", [Session]),
    
    Diff = timer:now_diff(Now, Timestamp),
    
    if
        % if session is older than 5 minutes, log it out
        Diff > 1000 * 1000 * 60 * 5 ->
            session_logout(Session, State);
        true -> ok
    end,
    
    % continue with the next session in the list
    internal_kill_timeouts(State, Now, ets:next(Tid, Session)).
    
    
session_logout(Session, _State = #state{table=Tid, timeout_table=TimeoutTable, session_table=SessionTid}) ->
    io:format("[SESSION] Logout ~s~n", [Session:get_name()]),
    
    % Remove the session from all channels
    chat_master:chat_kill(Session),
    
    % Clear the messages from the messages table
    ets:delete(SessionTid, Session),
    
    % Delete the current session
    ets:delete(Tid, Session:get_apikey()),
    
    % Delete the timeout
    ets:delete(TimeoutTable, Session),
    
    ok.