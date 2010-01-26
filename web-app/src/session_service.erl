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
find(SessionKey) ->
    Tid = t_session,
    
    case ets:member(Tid, SessionKey) of
        true ->
            [{SessionKey, _Username, Pid, _Timeout}] = ets:lookup(Tid, SessionKey),
            Pid;
        _ ->
            undefined
    end.
    
    %{ok, Pid} = gen_server:call(session_service, {find, SessionKey}),
    %Pid.
    
%% -----------------------------------------------------------------------------
%% Internal Api:
%
% Simply implementation: The state is a list of of {SessionKey, Pid} pairs. No sort, nothing
%
% TODO: Replace the tuple in the table with a record
%

init(_Args) ->
    Tid = ets:new(t_session, [set, protected, named_table]),
    
    {ok, Tid}.
    
handle_call({login, Username, Password}, _From, Tid) ->
    io:format("Login for user ~s~n", [Username]),
    
    case player_repository:check_login(Username, Password, []) of
        ok ->
            Key = ets:foldl(
                fun({SessionKey, ObjUsername, _Pid, _}, In) ->
                    case Username of
                        ObjUsername ->
                            SessionKey;
                        _ ->
                            In
                    end
                end,
                undefined,
                Tid
            ),
            
            Result = case Key of
                undefined ->
                    % Create a new session key
                    SessionKey = gen_sessionkey(),
                    
                    % Start the session
                    case session_handler:start_link(Username, SessionKey) of
                        {ok, SessionPid, Timeout} ->
                            ets:insert(
                                Tid,
                                {SessionKey, Username, SessionPid, Timeout}
                            ),
                            
                            {ok, list_to_binary(SessionKey), Timeout};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    [{SessionKey, Username, _SessionPid, Timeout}] = ets:lookup(Tid, Key),
                    {ok, list_to_binary(SessionKey), Timeout}
                    
            end,            
            {reply, Result, Tid};
        _ ->
            {reply, error, Tid}
    end;

handle_call({logoff, SessionKey}, _From, Tid) ->
    io:format("Logoff session ~s~n", [SessionKey]),
    
    ets:delete(Tid, SessionKey),
    
    {reply, ok, Tid};
    
handle_call({find, SessionKey}, _From, Tid) ->
    case ets:member(Tid, SessionKey) of
        true ->
            [{SessionKey, _Username, Pid, _Timeout}] = ets:lookup(Tid, SessionKey),
            {reply, {ok, Pid}, Tid};
        _ ->
            {reply, {ok, undefined}, Tid}
    end;
    
handle_call(_,_,State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
    
handle_cast(_,State) -> {noreply, State}.
handle_info(_,State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% -----------------------------------------------------------------------------
% Helper functions:
gen_sessionkey() ->
    gen_sessionkey(32).
    
gen_sessionkey(Length) ->
    lists:map(fun (_) -> random:uniform(24)+$\s+1 end, lists:seq(1,Length)).



    