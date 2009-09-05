-module(session_service).

-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% start_link() -> {ok, pid()}
start_link() ->
    gen_server:start_link({local, session_service}, ?MODULE, [], []).
    
%%
%%
%% login(Username, Password) -> {ok, Sessionkes} |Êerror
%% Default timeout
login(Username, Password) ->
    gen_server:call(session_service, {login, Username, Password}).
    
% logoff(Sessionkey) -> ok
logoff(Sessionkey) ->
    gen_server:call(session_service, {logoff, Sessionkey}).
    
%% find(Sessionkey) -> pid()
find(Sessionkey) ->
    {ok, Pid} = gen_server:call(session_service, {find, Sessionkey}),
    Pid.
    
    
%% -----------------------------------------------------------------------------
%% Internal Api:
%
% Simply implementation: The state is a list of of {SessionKey, Pid} pairs. No sort, nothing
%
% TODO: better state implementation
% TODO: Kick logged in users out


init(_Args) ->
    {ok, []}.
    
handle_call({login, Username, Password}, _From, State) ->
    io:format("Login for user ~s~n", [Username]),
    
    case check_username(Username, Password) of
        ok ->
            % Create a session key
            SessionKey = gen_sessionkey(),
            
            % Start the session
            {ok, SessionPid} = session_handler:start_link(Username),
            % Add these to the propertylist (our state)
            NewState = [proplists:property(SessionKey, SessionPid)] ++ proplists:delete(SessionKey,State),
            {reply, {ok, list_to_binary(SessionKey)}, NewState};
        _ ->
            {reply, error, State}
    end;

handle_call({logoff, SessionKey}, _From, State) ->
    io:format("Logoff user ~s~n", [proplists:lookup(SessionKey, State)]),
    NewState = proplists:delete(SessionKey, State),
    {reply, ok, NewState};
    
handle_call({find, SessionKey}, _From, State) ->
    Pid = proplists:get_value(SessionKey, State),
    {reply, {ok, Pid}, State};
    
handle_call(_,_,State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.
    
handle_cast(_,State) -> {noreply, State}.
handle_info(_,State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.




%% -----------------------------------------------------------------------------
% Helper functions:
check_username(Username, Password) ->
    % TODO: Validate the user + password
    ok.

gen_sessionkey() ->
    gen_sessionkey(32).
    
gen_sessionkey(Length) ->
    lists:map(fun (_) -> random:uniform(24)+$\s+1 end, lists:seq(1,Length)).



    