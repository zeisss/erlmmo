-module(zone_mob_event).


-export([
    send_field_info/3,
    send_map_info/2,
    send_zone_join/4,
    send_zone_part/3
]).

% TODO: Add the real field data here
send_field_info(Pid, Location, Mobs) ->
    Pid ! {event, {field_info, Location, Mobs}}.
    
send_map_info(Pid, MapInfo) ->
    Pid ! {event, {map_info, MapInfo}}.
    
    
send_zone_join(Pid, Username, IconUrl, Location) ->
    Pid ! {event, {zone_join, Username, IconUrl, Location}}.
    
send_zone_part(Pid, Name, Location) ->
    Pid ! {event, {zone_part, Name, Location}}.
    