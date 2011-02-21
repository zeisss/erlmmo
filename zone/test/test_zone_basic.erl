-module(test_zone_basic).

-include_lib("eunit/include/eunit.hrl").

basic_join_part_test() ->
    {error, {not_started, zone}} = zone:stop(),
    
    ok = zone:start(),
    
    % Setup ID information
    P1 = {p1, <<"P1">>},
    Z1 = zone1,
    
    % Join a zone
    ok = zone:join(Z1, P1, [
        {coordinate_hint, {0,0}}
    ]),
    
    ok = zone:part(Z1, P1),
    
    ok = zone:stop().
    
basic_join_part_event_test() ->
    {error, {not_started, zone}} = zone:stop(),
    
    ok = zone:start(),
    
    CollectorPid = test_utils:start_collector_loop(),
    
    % Setup ID information
    P1 = {p1, <<"P1">>},
    Z1 = zone1,
    CallbackFun = fun(ZoneId, PlayerId, Event) ->
        CollectorPid ! {ZoneId, PlayerId, Event}
    end,
    
    % Join a zone
    ok = zone:join(Z1, P1, [
        {coordinate_hint, {0,0}},
        {event_callback_fun, CallbackFun}
    ]),
    
    ok = zone:part(Z1, P1),
    
    ok = zone:stop(),
    
    receive
        after 2000 -> ok
    end,
    
    Messages = test_utils:get_messages(CollectorPid),
    
    [
        % Provides the basic "Hello to zone X", "MOTD" info, width, height
        {zone_welcome, ZoneId, <<"Unknown space">>, <<"">>, 1024, 1024},
        % Repetive (once a minute?) info about the current number of players in the current zone
        {zone_info, ZoneId, [{current_players, 1}]},
        % Whats the current status (what we see, what are we, ...) [once, every tick]
        % {messageId, Id, Position, Self, Others}
        {zone_status, ZoneId, {0,0}, {}, []},
        
        % When parting, we receive a last zone_final_status 
        {zone_final_status, ZoneId, {0,0}, {}}
    ] = Messages.
