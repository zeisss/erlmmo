-module(test_utils).
-export([start_collector_loop/0, get_messages/1, loop/0]).

% -> pid()
start_collector_loop() ->
    spawn(fun() -> loop() end).
    
    
get_messages(Pid) ->
    Pid ! {'$quit', self()},
    receive
        {'$get_messages', Messages} -> Messages
    end.
    
loop() ->
    loop([]).
    
loop(Messages) ->
    receive
        {'$quit', Pid} ->
            Pid ! {'$get_messages', Messages};
        M ->
            loop(Messages ++ [M])
    end.