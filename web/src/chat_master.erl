-module (chat_master).

-export([start_link/0, chat_list/1, chat_join/2, chat_send/3, chat_part/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).
-define(TABLE, chat_channels).

-record(state, {table}).

-record(channel, {name, sessions=[]}).


start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

chat_list(Session) ->
    gen_server:cast(?SERVER, {chat_list, Session}).

%%
% This fails silently, if the user was already in the given session.
chat_join(Session, Channel) ->
    gen_server:cast(?SERVER, {chat_join, Session, Channel}).
    
chat_send(Session, Channel, Message) ->
    gen_server:cast(?SERVER, {chat_send, Session, Channel, Message}).

chat_part(Session, Channel) ->
    gen_server:cast(?SERVER, {chat_part, Session, Channel}).

% ------------------------------------------------------------------------------

init([]) ->
    Tid = ets:new(?TABLE, [set, protected, {keypos,2}]),
    {ok, #state{table=Tid}}.
    
handle_cast({chat_list, Session}, State = #state{table=Tid}) ->
    Channels = ets:tab2list(Tid),
    ChannelNames = lists:map(fun(X) -> X#channel.name end, Channels),
    
    Session:add_message({chat_list, ChannelNames}),
    {noreply, State};
    
handle_cast({chat_join, Session, ChannelName}, State = #state{table=Tid}) ->
    Channel = case ets:lookup(Tid, ChannelName) of
        [A] -> A;
        [] -> #channel{name=ChannelName}
    end,
    OldSessions = Channel#channel.sessions,
    
    case lists:member(Session, OldSessions) of
        true -> ok;
        false ->
            NewSessions = OldSessions ++ [Session],
            NewChannel = Channel#channel{sessions=NewSessions},
            ets:insert(Tid, NewChannel),
            
            OtherNames = lists:map(fun(X) -> X:get_name() end, NewSessions),
            
            Session:add_message({chat_join, ChannelName, OtherNames})
    end,
    {noreply, State};

handle_cast({chat_send, Session, ChannelName, Message}, State = #state{table=Tid}) ->
    [Channel] = ets:lookup(Tid, ChannelName),
    lists:foreach(
        fun(UserSession) ->
            UserSession:add_message({chat_send, ChannelName, Session:get_name(), Message})
        end,
        Channel#channel.sessions
    ),
    {noreply, State};
    
handle_cast({chat_part, Session, ChannelName}, State = #state{table=Tid}) ->
    [Channel] = ets:lookup(Tid, ChannelName),
    NewSessions = lists:filter(
        fun(UserSession) -> not(UserSession =:= Session) end,
        Channel#channel.sessions
    ),
    ets:insert(Tid, Channel#channel{sessions=NewSessions}),
    {noreply, State}.
    

handle_call(_Message, _From, State) ->
    {noreply, State}.
    
handle_info(_Message, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
