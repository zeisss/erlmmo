-module(zone).
-behaviour(gen_server).

-record(state, {id, name, timer, objects, coords, paths}).

-define(TICK_TIME, timer:seconds(5)).

-include_lib("include/erlmmo.hrl").

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
    Name = proplists:get_value(name, PropList),
    
    zone_master:new_zone(ZoneId, self()),
    
    {ok, TimerRef} = case proplists:get_value(timer, PropList) of
        no_timer -> {ok, no_timer};
        _ -> timer:send_interval(?TICK_TIME, perform_tick)
    end,
    
    ObjectsTable = ets:new(zone_objects, [set, protected, {keypos, 2}]),
    CoordsTable = ets:new(zone_coords, [set, protected]),
    PathsTable = ets:new(zone_paths, [set, protected]),
    
    State = #state{id=ZoneId, name=Name, timer=TimerRef, objects=ObjectsTable, coords=CoordsTable, paths=PathsTable},
    
    init_space_objects(proplists:get_value(space_objects, PropList, []), State),
    
    {ok, State}.
    
init_space_objects([], _State) -> ok;
init_space_objects([Object | Rest], State = #state{objects=ObjectsTable}) ->
    
    Ref = make_ref(),
    
    ProtoType = proplists:get_value(prototype, Object),
    Coordinates = proplists:get_value(coordinates, Object),
    
    ets:insert(
        ObjectsTable,
        #zone_object{
            id = Ref,
            name = proplists:get_value(name, Object),
            prototype = zone_object_storage:load(ProtoType)
        }
    ),
    
    i_coords_add(Coordinates, Ref, State),
    
    init_space_objects(Rest, State).
    
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
    
handle_cast({session_join, Session, Coords}, State = #state{objects=OTid}) ->
    i_coords_add(Coords, Session, State),
    
    ets:insert(OTid, 
        #zone_object{
            id=Session,
            name=Session:get_name(),
            prototype=zone_object_storage:load(player_ship)
        }
    ),
    
    {noreply, State};
    
handle_cast({session_set_course, Session, Path}, State = #state{paths=PTid}) ->
    % This is simple, since a set can only have one object per key :D
    ets:insert(PTid, {Session, Path}),
    {noreply, State}.
    
handle_info(stop, State) ->
    {stop, normal,State};
    
handle_info(perform_tick, State = #state{id=ZoneId, paths=PTid, coords=CTid}) ->
    % io:format("[ZONE] Tick ~p~n", [ZoneId]),
    
    Coords = ets:tab2list(CTid),
    
    ok = internal_perform_tick(Coords, State),
    
    ok = broadcast_zone_status(ZoneId, State),
    
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
    
broadcast_zone_status(ZoneId, State = #state{coords=CTid, objects=OTid}) ->
    
    % First, generate a list of {Coord, ZoneObject}s
    ListOfCoordSessions = lists:flatten(
        lists:map(
            fun({Coord, References}) ->
                lists:map(
                    fun(Reference) ->
                        [Object] = ets:lookup(OTid, Reference),
                        {Coord, Object}
                    end,
                    References)
            end,
            ets:tab2list(CTid)
        )
    ),
    % now we have ListOfCoordSessions = [{Coord, ZoneObject}]
    
    lists:foreach(
        fun({_Coord, Object} = Self) ->
            case zone_object:can_receive_status(Object) of
                false -> ok;
                true ->
                    % Calculate which ships this Object can see
                    SeeableObjects = calculate_seeable_objects(ListOfCoordSessions, Self),
                    
                    % Send it to him.
                    zone_object:send_status(Object, SeeableObjects)
            end
        end,
        ListOfCoordSessions
    ),
    ok.

calculate_seeable_objects(ListOfCoordSessions, {{CX,CY}, SelfObject}) ->
    lists:filter(
        fun({{X,Y}, Object}) ->
            SelfProto = zone_object:prototype(Object),
            Proto = zone_object:prototype(SelfObject),
            zone_object:can_see({X,Y}, Proto#zone_object_prototype.size, {CX, CY}, SelfProto#zone_object_prototype.size, 11)
        end,
        ListOfCoordSessions
    ).
        
        
    
internal_perform_tick([], _) -> ok;
internal_perform_tick([{Coords, Sessions} | OtherCoords], State) ->
    % Perform all moves
    ok = internal_perform_tick_sessions(Coords, Sessions, State),
    internal_perform_tick(OtherCoords, State).

% Iterate over all sessions per coordinate
internal_perform_tick_sessions(_, [], _) -> ok;
internal_perform_tick_sessions(Coords, [Session| Sessions], State = #state{paths=PTid}) ->
    % Check if the session wants to move
    case ets:lookup(PTid, Session) of
        [] -> ok; % No, nothing to do
        [{Session, []}] ->
            ok; 
        [{Session, Paths}] ->
            [NextCoord|RestCoords] = Paths,
            
            case validation:validate_next_coords(NextCoord, Coords) of
                true ->
                    % Store the path coords, we do not handle
                    case RestCoords of
                        [] ->
                            ets:delete(PTid, Session);
                        _ ->
                            ets:insert(PTid, {Session, RestCoords})
                    end,
                    
                    % Ok, now we want to move the session to 'NextCoord':
                    
                    % 1) delete him from his current coords
                    i_coords_delete(Coords, Session, State),
                    
                    % 2) insert into the new coords
                    i_coords_add(NextCoord, Session, State);
                false ->
                    error_logger:error_msg("[ZONE] Session ~p tried to perform an illegal move (~p to ~p)", [Session, Coords, NextCoord])
            end
                    
    end,
    
    internal_perform_tick_sessions(Coords, Sessions, State).
    
% TODO: We could redesign the coords table to be a bag or so.
i_coords_add(Coords = {_,_}, Reference, _State = #state{coords=CTid}) ->
    case ets:lookup(CTid, Coords) of
        [] -> ets:insert(CTid, {Coords, [Reference]});
        [{Coords, Sessions}] ->
            NewSessions = Sessions ++ [Reference],
            ets:insert(CTid, {Coords, NewSessions})
    end.
    
i_coords_delete(Coords = {_,_}, Reference, _State = #state{coords=CTid}) ->
    [{Coords, AllSessions}] = ets:lookup(CTid, Coords),
    case lists:delete(Reference, AllSessions) of
        [] ->
            ets:delete(CTid, Coords);
        NewSessions ->
            ets:insert(CTid, {Coords, NewSessions})
    end.
    
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
            {{0,1}, _}
        ]
    }]} = S:get_messages_once(),
    
    Pid ! stop,
    
    ok.