-module(zone_master).

-behaviour(gen_server).

%%%
% Zone_Master:
%  This module forwards messages from zones to the right zone.
%
% Tables:
%  zone_pids - Contains {zone_id, pid()} touples for finding the right zone to a message.
%%

% Zone API
-export([new_zone/1, new_zone/2]).

% Session API
-export([zone_join/3, zone_set_course/2, kill_session/1, chat_send/2]).

% Gen_Server API
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ERROR_30001, {error, 30001, <<"???">>}).

-define(SERVER, {global, ?MODULE}).
-record(state, {zone_pids, zone_sessions}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This adds a zone to the internal list of zones
new_zone(ZoneId) ->
    new_zone(ZoneId, self()).
new_zone(ZoneId, Pid) ->
    gen_server:call(?SERVER, {new_zone, ZoneId, Pid}).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Session Api:


%%%
% Joins the zone with the given id.
%
% zone_join(Session, ZoneId, Coords) -> ok
%  Session = session_master:find(ApiKey)
%  ZoneId = term()
%  Coords = {X,Y}
%  X = Y = int()
zone_join(Session, ZoneId, Coords) ->
    gen_server:cast(?SERVER, {session_login, Session, ZoneId, Coords}).
    
chat_send(Session, Message) ->
    gen_server:cast(?SERVER, {chat_send, Session, Message}).
    
%%%
% Set the course for this session along the given path.
% The path is a list of coordinates which the zone should move the ship each tick along.
%
% zone_set_course(Session, Path) -> ok
%  Session = session_master:find(ApiKey)
%  Path = [{X, Y}]
zone_set_course(Session, Path) ->
    gen_server:cast(?SERVER, {session_set_course, Session, Path}).
    
%%%
% Removes the session from the internal list of {session,zone}s
kill_session(Session) ->
    gen_server:cast(?SERVER, {session_logout, Session}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General API:

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL API:
init([]) ->
    ZonePids = ets:new(zone_pids, [set, protected]),
    SessionZones = ets:new(zone_sessions, [set, protected]),
    
    State = #state{zone_pids=ZonePids, zone_sessions=SessionZones},
    {ok, State}.
    
handle_call({new_zone, ZoneId, Pid}, _From, State = #state{zone_pids=ZonePids, zone_sessions=_ZoneSessions}) ->
    case ets:lookup(ZonePids, ZoneId) of
        [] ->
            ets:insert(ZonePids, {ZoneId, Pid});
            
        [_ZoneOldPid] -> % Oooops, we already have this zone. Lets migrate all existing sessions
            % TODO: we need to migrate/delete the sessions for the old zone pid
            ets:insert(ZonePids, {ZoneId, Pid})
    end,    
    {reply, ok, State}.
    
handle_cast({chat_send, Session, Message}, State = #state{zone_sessions=ZoneSessions}) ->
    case ets:lookup(ZoneSessions, Session) of
        [] -> % No zone with this ID
            error_logger:error_msg("[ZONEMASTER] Session ~p has no zone. Must join a zone first.", [Session]),
            ok;
        [{Session, ZonePid}] -> % Found the zone
            zone:chat_send(ZonePid, Session, Message),
            ok
    end,
    {noreply, State};
    

    
handle_cast({session_login, Session, ZoneId, Coords}, State = #state{zone_pids=ZonePids, zone_sessions=ZoneSessions}) ->
    case ets:lookup(ZonePids, ZoneId) of
        [] -> % No zone with this ID
            error_logger:error_msg("[ZONEMASTER] Session ~p tried to join non-existing zone ~p", [Session, ZoneId]),
            ok;
        [{ZoneId, ZonePid}] -> % Found the zone
            zone:session_join(ZonePid, Session, Coords),
            
            ets:insert(ZoneSessions, {Session, ZonePid}),
            
            ok
    end,
    {noreply, State};
    
handle_cast({session_set_course, Session, Path}, State = #state{zone_sessions=ZoneSessions}) ->
    case ets:lookup(ZoneSessions, Session) of
        [] -> % No zone with this ID
            error_logger:error_msg("[ZONEMASTER] Session ~p has no zone. Must join a zone first.", [Session]),
            ok;
        [{Session, ZonePid}] -> % Found the zone
            zone:session_set_course(ZonePid, Session, Path),
            ok
    end,
    {noreply, State};
    
handle_cast({session_logout, Session}, State = #state{zone_sessions=ZoneSessions}) ->
    case ets:lookup(ZoneSessions, Session) of
        [{Session, ZonePid}] ->
            zone:session_kill(ZonePid, Session),
            ets:delete(ZoneSessions, Session);
        [] ->
            ok
    end,
    {noreply, State}.
        
    
    
    
handle_info(_, State) ->
    {noreply, State}.
    

terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
