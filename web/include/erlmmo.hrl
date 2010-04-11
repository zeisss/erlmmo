
%%%
% Prototype for zone_objects.
-record(zone_object_prototype, {
    name, % name = [char()] - Initial name
    description, % description = binary() - Longer description of this space object
    size, % size = int(). 1 for small ships / 40+ for planets / 100 Suns
    image % image = [char()] - Image to use for rendering
}).