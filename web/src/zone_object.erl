-module(zone_object).

-include_lib("include/erlmmo.hrl").

-export([is_player_ship/1, can_receive_status/1, send_status/3, send_info/2]).

-export([can_see/5, get_range/2, prototype/1, test/0]).

%%
% Shall the given ZoneObject receive the zone_status message every tick?
can_receive_status(ZoneObject) ->
    is_player_ship(ZoneObject).

send_info(Object, Name) when not(is_reference(Object#zone_object.id))->
    Session = Object#zone_object.id,
    
    Session:add_message(
        {zone_info, Name}
    ).
    
send_status(Object, Coord, VisibleObjects) when not(is_reference(Object#zone_object.id)) ->
    Session = Object#zone_object.id,
    
    Session:add_message (
        {zone_status, Coord, Object, VisibleObjects}
    ).


is_player_ship(ZoneObject = #zone_object{}) ->
    not(is_reference(ZoneObject#zone_object.id));
is_player_ship(_) ->
    false.
    
get_range({X0, Y0}, {X1, Y1}) ->
    A = abs(X0 - X1),
    B = abs(Y0 - Y1),
    math:sqrt(math:pow(A, 2) + math:pow(B,2)).
    
can_see(_, SizeOther, _, _, _) when SizeOther >= 40 -> true;
can_see(CoordsOther, SizeOther, CoordsSelf, SizeSelf, MaxRange) when is_integer(SizeOther), is_integer(SizeSelf) ->
    % Pythagoras
    % a*a + b*b = c*c
    % with a = abs(X1 - X2)
    %      b = abs(Y1 - Y2)
    %      c = the range between CoordsA and CoordsB
    %
    Range = get_range(CoordsOther, CoordsSelf),
    
    SizeOther + MaxRange + SizeSelf > Range.
    

prototype(ZoneObject) ->
    ZoneObject#zone_object.prototype.
    
    
test() ->
    true  = can_see({0,0}, 1, {10,10}, 2, 13),
    true  = can_see({0,0}, 1, {10,10}, 2, 12),
    false = can_see({0,0}, 1, {10,10}, 2, 11),
    false = can_see({0,0}, 1, {10,10}, 2, 10),
    false = can_see({0,0}, 1, {10,10}, 2, 9),
    
    true  = can_see({0,0}, 50, {100,100}, 1, 10),
    ok.
    