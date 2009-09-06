-ifndef(ERLMMO_HRL).
-define(ERLMMO_HRL, "erlmmo.hrl").

-record(app_config, {zone_files="priv/zones/"} ).

% Used for zone:join()
-record(zone_mob_info, {
    name,   % The 
    pid,    % The pid of the mob process
    location = {0,0}, % The location
    % ---------------------------------------
    uid    % A unique id set by the zone for internal reference
}).


-endif.
