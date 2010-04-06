-module (session_master).

-export([start_link/0, login/2, logout/1, find/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([session_add_message/2, session_get_messages/1]).

-record(state, {session_counter=0, table, session_table}).

-define(SERVER, {global, ?MODULE}).

-define(TABLE, sessions).


start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

%%
% Checks the credentials for the given player and starts a new session.
% The sessionid for the new session is returned.
% If the user was previously logged in, he will be logged out.
%
% login(string(), string()) -> {ok, ApiKey} |Ê{error, ErrorMessage}
% 
login(Name, Password) ->
    gen_server:call(?SERVER, {login, Name, Password}).

%
% Logs the given session out.
logout(ApiKey) ->
    gen_server:call(?SERVER, {logout, ApiKey}).
    
%
% get(apikey()) -> {ok, Session}
% Session:
find(ApiKey) ->
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
    SessionTid = ets:new(session_messages, [set, protected]),
    
    {ok, #state{table=Tid, session_table=SessionTid}}.
    
handle_call({login, Name, Password}, From, State = #state{table=Tid}) ->
    case ets:match_object(Tid, {session, '_', Name}) of
        [Session] -> handle_call({logout, Session:get_apikey()}, From, State);
        [] -> ok
    end, 
    
    Counter = State#state.session_counter,
    
    % TODO: Create a real random apikey here
    ApiKey = erlang:list_to_binary([<<"TODO:APIKEY">>, erlang:list_to_binary(erlang:integer_to_list(Counter))]),
    
    NewSession = session:new(ApiKey, Name),
    case NewSession:init() of
        ok ->
            ets:insert(Tid, NewSession),
            
            {reply, {ok, ApiKey}, State#state{session_counter=Counter+1}};
        _Error ->
            {reply, {error, failed_while_init_session}, State}
    end;
    
handle_call({logout, ApiKey}, _From, State = #state{table=Tid}) ->
    ets:delete(Tid, ApiKey),
    {reply, ok, State};
    
handle_call({find, ApiKey}, _From, State = #state{table=Tid}) ->
    case ets:lookup(Tid, ApiKey) of
        [Session] -> {reply, Session, State};
        [] -> {reply, no_session, State};
        Result -> {stop, {multiple_sessions_for_apikey, ApiKey, Result}, no_session, State}
    end;
    
    
handle_call({session_add_message, Session, Message}, _From, State = #state{session_table=Tid}) ->
    Messages = case ets:lookup(Tid, Session) of
        [] -> [];
        [{Session, Msgs}] -> Msgs;
        _ -> []
    end,
    
    ets:insert(Tid, {Session, Messages ++ [Message]}),
    {reply, ok, State};
    
handle_call({session_get_messages, Session}, _From, State = #state{session_table=Tid}) ->
    Messages = case ets:lookup(Tid, Session) of
        [] -> [];
        [{Session, Msgs}] -> Msgs
    end,
    true = ets:delete(Tid, Session),
    
    {reply, {ok, Messages}, State}.    
    
handle_cast(_Message, State) ->
    {noreply, State}.
    
handle_info(_Message, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
