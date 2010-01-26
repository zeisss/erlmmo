
-module(player_repository).
-include("records.hrl").

-export([start_link/0, check_login/3, fetch/1, store/1]).

start_link() ->
    ok. % gen_server:start_link({local, player_repository}, ?MODULE, [], []).
    
check_login(_Username, _Password, _Options) ->
    ok.
    
fetch(Username) ->
    {ok, #player{
        username=Username, % Player Name
        zone='0_0_0_forest.yml', % Current zone
        location={0,0}           % Location
    }}.
    
store(_Player) ->
    ok.