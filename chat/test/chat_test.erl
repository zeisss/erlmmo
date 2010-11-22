-module(chat_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() -> 
  chat:start(),

  chat:connect(user1, []),
  chat:connect(user2, []),

  chat:join(user1, global, []),
  chat:join(user2, global, []),

  chat:send(user1, global, hi),
  chat:send(user2, global, <<"Hi User1">>),

  chat:part(user1, global, [{reason, client_quit}]),

  chat:send(user2, global, <<"Wait!">>),
  chat:part(user2, global, []),
  ok.
