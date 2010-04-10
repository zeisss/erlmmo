-module(zone).
-behaviour(gen_server).

-record(state, {id, timer, coords, paths}).

-define(TICK_TIME, timer:seconds(5)).

% Other
-export([perform_tick/1, test/0]).

% Session API
-export([session_join/3, session_set_course/3, session_kill/2]).

% Gen_Server API
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

session_join(ZonePid, Session, Coords) ->
    gen_server:cast(ZonePid, {session_join, Session, Coords}).
    
session_set_course(ZonePid, Session, Path) ->
    gen_server:cast(ZonePid, {session_set_course, Session, Path}).
    
session_kill(ZonePid, Session) ->
    gen_server:cast(ZonePid, {session_kill, Session}).
    
perform_tick(ZonePid) ->
    ZonePid ! perform_tick,
    ok.
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(PropList) ->
    gen_server:start_link(?MODULE, PropList, []).
    
init(PropList) ->
    ZoneId = proplists:get_value(id, PropList),
    
    zone_master:new_zone(ZoneId, self()),
    
    {ok, TimerRef} = case proplists:get_value(timer, PropList) of
        no_timer -> {ok, no_timer};
        _ -> timer:send_interval(?TICK_TIME, perform_tick)
    end,
    
    CoordsTable = ets:new(zone_coords, [set, protected]),
    PathsTable = ets:new(zone_paths, [set, protected]),
    
    State = #state{id=ZoneId, timer=TimerRef, coords=CoordsTable, paths=PathsTable},
    
    {ok, State}.
    
handle_call(_Message, _From, State) ->
    {noreply, State}.
    
handle_cast({session_kill, Session}, State = #state{coords=CTid, paths=PTid}) ->
    ets:delete(PTid, Session),
    
    List = ets:tab2list(CTid),
    
    lists:foreach(
        fun({Coords, Sessions}) ->
            case lists:member(Session, Sessions) of
                true ->
                    NewList = lists:delete(Session, Sessions),
                    ets:insert(CTid, {Coords, NewList});
                false ->
                    ok
            end
        end, List
    ),
    
    {noreply, State};
    
handle_cast({session_join, Session, Coords}, State = #state{coords=CTid}) ->
    % TODO: We could redesign the coords table to be a bag or so.
    
    case ets:member(CTid, Coords) of
        false -> % Coords do not exist, simple write
            ets:insert(CTid, {Coords, [Session]});
        true -> % Coords exist, we have to read, manipulate, write
            [{Coords, Sessions}] = ets:lookup(CTid, Coords),
            
            NewSessions = Sessions ++ [Session],
            
            ets:insert(CTid, {Coords, NewSessions})
    end,
    {noreply, State};
    
handle_cast({session_set_course, Session, Path}, State = #state{paths=PTid}) ->
    % This is simple, since a set can only have one object per key :D
    ets:insert(PTid, {Session, Path}),
    {noreply, State}.
    
handle_info(stop, State) ->
    {stop, normal,State};
    
handle_info(perform_tick, State = #state{id=ZoneId, paths=PTid, coords=CTid}) ->
    io:format("Tick ~p~n", [ZoneId]),
    
    Coords = ets:tab2list(CTid),
    
    ok = internal_perform_tick(Coords, PTid, CTid),
    
    ok = internal_send_zone_status(CTid),
    
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer of
        no_timer -> ok;
        _ ->
            {ok, cancel} = timer:cancel(State#state.timer)
    end,
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% -----------
    
internal_send_zone_status(CTid) ->
    
    ListOfCoordSessions = lists:flatten(
        lists:map(
            fun({Coord, Sessions}) ->
                lists:map(fun(Session) -> {Coord, Session} end, Sessions)
            end,
            ets:tab2list(CTid)
        )
    ),
    % now we have ListOfCoordSessions = [{Coord, Session}]
    
    lists:foreach(
        fun({Coord, Session}) ->
            SeeableObjects = internal_seeable_objects(ListOfCoordSessions, Coord),
            
            Session:add_message ({
                zone_status,
                SeeableObjects
            })
        end,
        ListOfCoordSessions
    ),
    ok.

internal_seeable_objects(ListOfCoordSessions, {CX,CY}) ->
    lists:filter(
        fun({{X,Y}, _}) ->
            abs(CX-X) < 11 andalso abs(CY - Y) < 11
        end,
        ListOfCoordSessions
    ).
        
        
    
internal_perform_tick([], _, _ ) -> ok;
internal_perform_tick([{Coords, Sessions} | OtherCoords], PTid, CTid) ->
    % Perform all moves
    ok = internal_perform_tick_sessions(Coords, Sessions, PTid, CTid),
    internal_perform_tick(OtherCoords, PTid, CTid).

% Iterate over all sessions per coordinate
internal_perform_tick_sessions(_, [], _,_) -> ok;
internal_perform_tick_sessions(Coords, [Session| Sessions], PTid, CTid ) ->
    % Check if the session wants to move
    case ets:lookup(PTid, Session) of
        [] -> ok; % No, nothing to do
        [{Session, Paths}] ->
            [NextCoord|RestCoords] = Paths,
            
            case validation:validate_next_coords(NextCoord, Coords) of
                true ->
                    % Store the path coords, we do not handle
                    ets:insert(PTid, {Session, RestCoords}),
                    
                    % Ok, now we want to move the session to 'NextCoord':
                    
                    % 1) delete him from his current coords
                    [{Coords, AllSessions}] = ets:lookup(CTid, Coords),
                    NewSessions = lists:delete(Session, AllSessions),
                    ets:insert(CTid, {Coords, NewSessions}),
                    
                    % 2) insert into the new coords
                    case ets:lookup(CTid, NextCoord) of
                        [] -> ets:insert(CTid, {NextCoord, [Session]});
                        [{NextCoord, NextSessions}] ->
                            NewNextSessions = NextSessions ++ [Session],
                            ets:insert(CTid, {NextCoord, NewNextSessions})
                    end;
                false ->
                    error_logger:error_msg("Session ~p tried to peform an illegal move (~p to ~p)", [Session, Coords, NextCoord])
            end
                    
    end,
    
    
    internal_perform_tick_sessions(Coords, Sessions, PTid, CTid).
    
    
test() ->
    {ok, Pid} = start_link([{id, zone_test}, {timer, no_timer}]),
    {ok, ApiKey} = session_master:login(<<"Miro">>, no_password),
    {ok, S} = session_master:find(ApiKey),
    
    timer:sleep(500),
    
    {ok, _Messages} = S:get_messages_once(),
    
    zone:session_join(Pid, S, {0,0}),
    
    
    {ok, []} = S:get_messages_once(),
    
    zone:session_set_course(Pid, S, [{0,1}]),
    zone:perform_tick(Pid),
    timer:sleep(500),
    {ok, [{zone_status,
        [
            {{0,1}, S}
        ]
    }]} = S:get_messages_once(),
    
    Pid ! stop,
    
    ok.