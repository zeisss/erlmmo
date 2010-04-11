-module(zone).
-behaviour(gen_server).

-record(state, {id, name, timer, objects, coords, paths}).

-define(TICK_TIME, timer:seconds(5)).

-include_lib("include/erlmmo.hrl").

%%%
% The zone object provides general information about the object in space.
% It is referenced in the zone_coords table, where the id is used to reference
% to such an object.
% For session objects, the session is used.
-record(zone_object, {
    id, % id = make_ref() | Session
    name, % name = [char()] - Name to use on starmap 
    control, % control = sessionÊ|Ênpc
    prototype, % prototype = zone_object_prototype()
    speed = 0 % speed = integer() - positiv: how many fields per second - negativ: How many ticks per field.
}).


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
            control = npc,
            prototype = zone_object_storage:load(ProtoType),
            speed = 0
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
    
handle_cast({session_join, Session, Coords}, State = #state{coords=CTid}) ->
    i_coords_add(Coords, Session, State),
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
    
    ok = internal_send_zone_status(ZoneId, State),
    
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
    
internal_send_zone_status(ZoneId, State = #state{coords=CTid, objects=OTid}) ->
    
    ListOfCoordSessions = lists:flatten(
        lists:map(
            fun({Coord, References}) ->
                lists:map(
                    fun(Reference) ->
                        [Object] = ets:lookup(OTid, Reference),
                        {Coord, Reference, Object}
                    end,
                    References)
            end,
            ets:tab2list(CTid)
        )
    ),
    % now we have ListOfCoordSessions = [{Coord, Session}]
    
    lists:foreach(
        fun({_Coord, Session, Object} = Self) ->
            case Object#zone_object.control of
                npc -> ok;
                session ->
                    SeeableObjects = internal_seeable_objects(ListOfCoordSessions, Self),
                    
                    Session:add_message ({
                        zone_status,
                        SeeableObjects
                    })
            end
        end,
        ListOfCoordSessions
    ),
    ok.

internal_seeable_objects(ListOfCoordSessions, {{CX,CY}, _, SelfObject}) ->
    lists:filter(
        fun({{X,Y}, _, Object}) ->
            SelfProto = Object#zone_object.prototype,
            Proto = SelfObject#zone_object.prototype,
            i_can_see({X,Y}, Proto#zone_object_prototype.size, {CX, CY}, SelfProto#zone_object_prototype.size, 11)
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
        [{Session, Paths}] ->
            [NextCoord|RestCoords] = Paths,
            
            case validation:validate_next_coords(NextCoord, Coords) of
                true ->
                    % Store the path coords, we do not handle
                    ets:insert(PTid, {Session, RestCoords}),
                    
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
    
% TODO: This can surely be optimized
i_can_see(_CoordsA = {X1,Y1}, SizeA, _CoordsB = {X2, Y2}, SizeB, MaxRange) when is_integer(SizeA), is_integer(SizeB) ->
    % Pythagoras
    % a*a + b*b = c*c
    % with a = abs(X1 - X2)
    %      b = abs(Y1 - Y2)
    %      c = the range between CoordsA and CoordsB
    %
    A = abs(X1 - X2),
    B = abs(Y1 - Y2),
    C = math:sqrt(math:pow(A, 2) + math:pow(B,2)),
    
    SizeA + MaxRange + SizeB > C.
    
    
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