-module(zone).

% Startup / Tear Down
-export([
    start/0,
    start_link/0,
    stop/0
]).

% Basic interaction
-export([
    join/3,
    part/2
]).

-define(ZONE_PID_TABLE, zone_pids).

% ------------------------------------------------------------------------------

start() ->
    application:start(zone).
    
start_link() ->
    zone_app:start(undefined, undefined).
    
%%%
% Stops the zone-application, if it was started with 'start()' before.
%
stop() ->
    application:stop(zone).
    
%------------------------------------------------------------------------------
% Adds the given ID to object list of the specified zone.
%
% ZoneId = atom()
% PlayerId = term()
% Options = [Option]
% Option = {OptionKey, OptionValue}
% OptionKey = event_callback_fun, OptionValue=Fun/3 (ZoneId, PlayerId, Event)
%%
join(ZoneId, PlayerId, Options) ->
    ZonePid = ets:lookup_element(?ZONE_PID_TABLE, ZoneId, 2),
    case ZonePid of
        undefined ->
            {error, unknown_zone};
        _ ->
            zone_server:join(ZonePid, PlayerId, Options)
    end.
    
%%%
% Removes the player from the given zone.
% 
%
part(ZoneId, PlayerId) ->
    ZonePid = ets:lookup_element(?ZONE_PID_TABLE, ZoneId, 2),
    case ZonePid of
        undefined ->
            {error, unknown_zone};
        _ ->
            zone_server:part(ZonePid, PlayerId)
    end.

