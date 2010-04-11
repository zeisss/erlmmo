-module(zone_object_storage).

-include_lib("include/erlmmo.hrl").

-export([load/1]).

load(normal_sun) ->
    #zone_object_prototype{
        name = <<"Sun">>,
        description = <<"A sun of normal size. It's flames are glaring in a hot red">>,
        size = 50,
        image = "normal_sun.png"
    };
    
load(small_planet_m) ->
    #zone_object_prototype{
        name = <<"Planet (M)">>,
        description = <<"A small planet of class M, able to habitate human life.">>,
        size = 15,
        image = "small_planet_m.png"
    }.