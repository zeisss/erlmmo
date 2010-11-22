-module(chat_test).

-include_lib("eunit/include/eunit.hrl").

loop(Messages) ->
    NewMessages = receive
        {quit, Pid} = M ->
            io:format("---~n~w~n", [M]),
            io:format("~w~n---~n", [Messages]),
            Pid ! {messages, Messages},
            ok;
        Message ->
            io:format("~w~n", [Message]),
            N = lists:append(Messages, [Message]),
            loop(N)
    end.
    
simple_test() ->
  Pid = spawn(fun() ->
    loop([])
  end),
  
  ok = chat:start(),

  ok = chat:connect(user1, [{callback, fun(X) -> Pid ! X end}]),
  ok = chat:connect(user2, []),

  ok = chat:join(user1, global, []),
  ok = chat:join(user2, global, []),

  ok = chat:send(user1, global, hi),
  ok = chat:send(user2, global, <<"Hi User1">>),

  ok = chat:part(user1, global, [{reason, client_quit}]),

  ok = chat:send(user2, global, <<"Wait!">>),
  ok = chat:part(user2, global, []),
  
  % 
  
  receive
    after 2000 -> ok
  end,
    
  Pid ! {quit, self()},
  
  [
    {chat_channel_join,global,[]},
    {chat_join,global,user2},
    {chat_part,global,user1,client_quit}
  ] = receive 
    {messages, M} -> M;
    _ -> no_match
  end,
  
  ok = chat:stop(),
  
  ok.
