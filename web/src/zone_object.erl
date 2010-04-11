-module(zone_object).

-include_lib("include/erlmmo.hrl").

-export([is_player_ship/1, can_receive_status/1, send_status/2]).

-export([can_see/5, prototype/1, test/0]).

%%
% Shall the given ZoneObject receive the zone_status message every tick?
can_receive_status(ZoneObject) ->
    is_player_ship(ZoneObject).

send_status(Object, Message) when not(is_reference(Object#zone_object.id)) ->
    Session = Object#zone_object.id,
    
    Session:add_message (
        {zone_status, Message}
    ).


is_player_ship(ZoneObject) ->
    not(is_reference(ZoneObject#zone_object.id)).
    
can_see(_, SizeOther, _, _, _) when SizeOther >= 40 -> true;
can_see(_CoordsOther = {X1,Y1}, SizeOther, _CoordsSelf = {X2, Y2}, SizeSelf, MaxRange) when is_integer(SizeOther), is_integer(SizeSelf) ->
    % Pythagoras
    % a*a + b*b = c*c
    % with a = abs(X1 - X2)
    %      b = abs(Y1 - Y2)
    %      c = the range between CoordsA and CoordsB
    %
    A = abs(X1 - X2),
    B = abs(Y1 - Y2),
    C = math:sqrt(math:pow(A, 2) + math:pow(B,2)),
    
    SizeOther + MaxRange + SizeSelf > C.
    

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
    