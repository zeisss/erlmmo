
%%%
% Prototype for zone_objects.
-record(zone_object_prototype, {
    name, % name = binary() - Initial name
    description, % description = binary() - Longer description of this space object
    size, % size = int(). 1 for small ships / 40+ for planets / 100 Suns
    image % image = [char()] - Image to use for rendering
}).



%%%
% The zone object provides general information about the object in space.
% It is referenced in the zone_coords table, where the id is used to reference
% to such an object.
% For session objects, the session is used.
-record(zone_object, {
    id, % id = make_ref() | Session
    name, % name = [char()] - Name to use on starmap
    prototype % prototype = zone_object_prototype()
}).

