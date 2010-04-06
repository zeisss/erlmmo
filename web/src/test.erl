-module(test).

-compile(export_all).


start() ->
    spawn(fun() -> test() end).
    
test() ->
    ok = test_login(),
    
    t(test_simple_messages),
    t(test_solo_chatting),
    t(test_multi_chatting).
    
test_login() ->    
    {ok, Miro} = login(),
    ok = logout(Miro).
    
test_simple_messages(Miro) ->
    % Test simple message handling
    {ok, []} = Miro:get_messages_once(),
    ok = Miro:add_message({test_message, "Hi"}),
    {ok, [{test_message, "Hi"}]} = Miro:get_messages_once(),
    {ok, []} = Miro:get_messages_once(),
    ok.
    
test_solo_chatting(Miro) ->
    ok = Miro:chat_join("Global"),
    timer:sleep(500),
    {ok, [{chat_join_self, "Global", ["Miro"]}]} = Miro:get_messages_once(),
    
    chat_master:chat_list(Miro),
    timer:sleep(500),
    {ok, [{chat_list, ["Global"]}]} = Miro:get_messages_once(),
    
    Miro:chat_send("Global", "Hello World!"),
    timer:sleep(500),
    {ok, [{chat_send, "Global", "Miro", "Hello World!"}]} = Miro:get_messages_once(),
    ok.
    
test_multi_chatting(Miro) ->
    % Ok, bring a second one into play
    {ok, ApiKeyAnda} = session_master:login("Andarya", "no_password"),
    {ok, Anda} = session_master:find(ApiKeyAnda),
    
    
    Miro:chat_join("Global"),
    timer:sleep(500),
    Miro:get_messages_once(),
    
    Anda:chat_join("Global"),
    
    timer:sleep(500),
    Anda:get_messages_once(), % Simple Clear, since already tested above
    {ok, [{chat_join, "Global", "Andarya"}]} = Miro:get_messages_once(),
    
    
    Anda:chat_send("Global", "Hi!"),
    timer:sleep(500),
    
    {ok, [{chat_send, "Global", "Andarya", "Hi!"}]} = Miro:get_messages_once(),
    {ok, [{chat_send, "Global", "Andarya", "Hi!"}]} = Anda:get_messages_once(),
    
    ok = logout(Anda),
    
    ok.
    
t(Function) ->
    
    io:format("~n~nTEST ~w~n~n", [Function]),
    session_master:logout_all(),
    
    {ok, Session} = login(),
    ok = apply(?MODULE, Function, [Session]),
    ok = logout(Session).
    
    
login() ->
    {ok, ApiKeyMiro} = session_master:login("Miro", "no_password"),
    {ok, Session} = session_master:find(ApiKeyMiro),
    io:format("Current session: ~p --------------~n", [Session]),
    {ok, Session}.
    
logout(Session) ->
    session_master:logout(Session:get_apikey()).