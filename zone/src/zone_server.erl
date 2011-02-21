-module(zone_server).

-export([init_table/0, start/1, join/3, part/2]).

%%%%
% A zone_server represents a actual zone, where players can move around and do stuff.
% 
%%%%

% Called by zone_app to create the 'global' PID tables
init_table() ->
    io:format("[zone] Created pid table~n"),
    
    ets:new(zone_pids, [
        set,
        public,
        named_table
    ]),
    ok.

start(Id) ->
    ok.
    
join(Pid, PlayerId, Options) ->
    ok.
    
part(Pid, PlayerId) ->
    ok.

% ----------------------

init(Id) ->
    {ok, Id}.