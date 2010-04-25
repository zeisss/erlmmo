
%%%
% Stored in the storage under 'account'
-record (account_info, {
    login_name,
    login_password, % hashed
    email,
    admin_level = 0 % 0 = user, 5 = Moderator-Channel, 10 = Dev-Channel
}).

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
    cargo = [], % The cargo, a list of {CargoType, Amount (n > 0)} tuples
    actions = [], % The actions users can perform (A list of {ActionType, Fun})
    prototype % prototype = zone_object_prototype()
}).






-record(skill_group, {
    groupId,
    groupName
}).

-record(skill, {
    skillId,
    name,
    description,
    requirement=[]
}).

-record(skill_level, {
    skillId,
    level
}).