-module(chat_test).

-include_lib("eunit/include/eunit.hrl").

% Helper method collecting all messages for later retrieval.
loop(Messages) ->
    NewMessages = receive
        {quit, Pid} = M ->
            io:format("---~n~w~n", [M]),
            io:format("~w~n---~n", [Messages]),
            Pid ! {messages, Messages},
            ok;
        Message ->
            io:format("<< ~w~n", [Message]),
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
  
  {ok, [global]} = chat:get_channels(user1),

  ok = chat:send(user1, global, hi),
  ok = chat:send(user2, global, <<"Hi User1">>),

  ok = chat:part(user1, global, [{reason, client_quit}]),

  ok = chat:send(user2, global, <<"Wait!">>),
  ok = chat:part(user2, global, []),
  
  ok = chat:whisper(user2, user1, whisper_message),
  
  ok = chat:disconnect(user2, []),
  ok = chat:disconnect(user1, [{reason, <<"Going to dinner">>}]),
  
  
  % Ok, now test the message we should have received in the callback.
  
  receive
    after 2000 -> ok
  end,
  
  Pid ! {quit, self()},
  
  [
    {chat_channel_join,global,[]},
    {chat_join,global,user2},
    {chat_send, global, user1, hi},
    {chat_send, global, user2, <<"Hi User1">>},
    {chat_part,global,user1,client_quit},
    {chat_whisper, user2, whisper_message},
    % NOTE: We might not receive this messages, because we are to fast with quitting :g
    % {chat_quit, user2, no_reason},
    % {chat_quit, user1, <<"Going to dinner">>},
    chat_disconnected
  ] = receive 
    {messages, M} -> M;
    _ -> no_match
  end,
  
  ok = chat:stop(),
  
  ok.

simple2_test() ->
    chat:start(),
    
    ok = chat:connect({session,<<"TODO:APIKEY0">>,<<"Miro">>}, [{callback, fun(X) -> io:format("~w~n", [X]) end}]),
    ok = chat:join({session,<<"TODO:APIKEY0">>,<<"Miro">>}, <<"Global">>, []),
    
    ok = chat:stop().