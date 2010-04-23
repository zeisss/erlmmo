-module(storage).

-include_lib("include/erlmmo.hrl").

-export([save_object/2, load_object/1]).

%%
% Stores the given Value in the internal binary format
% as a file.
% This value can be reloaded by calling load_object()
% with the same arguments.
%
% @see term_to_binary/1
%
% save_object(Key, Value) -> ok | {error, Reason}
% Key = Value = Reason = term()
%
save_object(Key = {session, Session, Attribute}, Value) ->    
    Filename = filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        "session",
        string:to_lower(binary_to_list(Session:get_name())),
        atom_to_list(Attribute)
    ]),
    
    Dirname = filename:dirname(Filename),
    
    case filelib:ensure_dir(Dirname) of
        ok ->
            Binary = term_to_binary(Value),
            file:write_file(Filename, Binary);
        Err -> Err
    end.
    
%%%
% load_object(Key) -> {ok, Result} | {error, Reason}
%
load_object(Key = {session, Session, Attribute}) when is_atom(Attribute) ->
    Filename = filename:join(
        [
            filename:dirname(code:which(?MODULE)),
            "..",
            "priv",
            "session",
            string:to_lower(binary_to_list(Session:get_name())),
            atom_to_list(Attribute)
        ] 
    ),
    
    
    Result = file:read_file(Filename),
    
    case Result of
        {error, _Reason} ->
            % io:format("~p~n", [Reason]),
            {ok, default_object(Key)};
        {ok, Binary} ->
            {ok, binary_to_term(Binary)}
    end;
                

load_object(normal_sun) ->
    #zone_object_prototype{
        name = <<"Sun">>,
        description = <<"A sun of normal size. It's flames are glaring in a hot red">>,
        size = 100,
        image = "normal_sun.png"
    };
    
load_object(small_planet_m) ->
    #zone_object_prototype{
        name = <<"Planet (M)">>,
        description = <<"A small planet of class M, able to habitate human life.">>,
        size = 40,
        image = "small_planet_m.png"
    };
    
load_object(asteroid_small) ->
    #zone_object_prototype{
        name = <<"Asteroid">>,
        description = <<"Asteroid Small">>,
        size = 1,
        image = "asteroid.png"
    };
load_object(asteroid_huge) ->
    #zone_object_prototype{
        name = <<"Asteroid">>,
        description = <<"Asteroid Huge">>,
        size = 10,
        image = "asteroid.png"
    };
    
load_object(player_ship) ->
    #zone_object_prototype{
        name = <<"Player Ship (Mark I)">>,
        description = <<"A small vessel designed to touch outer space.">>,
        size=1,
        image="small_ship.png"
    }.
    
    
default_object(_) ->
    undefined.