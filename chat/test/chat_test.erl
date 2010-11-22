-module(chat_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() -> 
  ok = chat:start(),

  ok = chat:connect(user1, []),
  ok = chat:connect(user2, []),

  ok = chat:join(user1, global, []),
  ok = chat:join(user2, global, []),

  ok = chat:send(user1, global, hi),
  ok = chat:send(user2, global, <<"Hi User1">>),

  ok = chat:part(user1, global, [{reason, client_quit}]),

  ok = chat:send(user2, global, <<"Wait!">>),
  ok = chat:part(user2, global, []),
  
  % ok = chat:stop(),
  ok.
