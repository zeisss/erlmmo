-module (chat_master).

-export([status/0, channel_list/0]).
-export([start_link/0, chat_join/2, chat_send/3, chat_part/2, chat_kill/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%
% Error Handling:
% There are two types or errors that can occur, that we handle:
%  illegal error: This error occures, when the user plays around with the api, which he is not
%                 supposed to do.
%                 These errors are logger to the 'error_logger' module. The user action is
%                 silently (for the user) ignored.
%  user error: When a user error occures, the user did something wrong / what we don't allow.
%              e.g. sending an empty chat message or joining a channel with illegal characters
%              When this happens, we send the user an error message from the list below.
%              The error codes shall be unique. The range for the chat_master is 20.000 - 29.999
%%
-define(ERROR_20001, {error, 20001, <<"Invalid channelname. Unable to join.">>}).
-define(ERROR_20002, {error, 20002, <<"You are already in this channel.">>}).
-define(ERROR_20003, {error, 20003, <<"Invalid chat message.">>}).

-define(SERVER, {global, ?MODULE}).
-define(TABLE, chat_channels).

-record(state, {table}).

-record(channel, {name, sessions=[]}).


start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).
    

status() ->
    gen_server:cast(?SERVER, status).


%%
% Returns an array of channel names.
%
% chat_list() -> {ok, [ChannelName]}
%    ChannelName = binary()
channel_list() ->
    gen_server:call(?SERVER, channel_list).

%%
% This fails silently, if the user was already in the given session.
chat_join(Session, Channel) when is_binary(Channel) ->
    gen_server:cast(?SERVER, {chat_join, Session, Channel}).
    
chat_send(Session, Channel, Message) when is_binary(Channel) andalso is_binary(Message)->
    gen_server:cast(?SERVER, {chat_send, Session, Channel, Message}).

chat_part(Session, Channel) when is_binary(Channel) ->
    gen_server:cast(?SERVER, {chat_part, Session, Channel}).
    
%%
% Removes the given sesson from all chat rooms
chat_kill(Session) ->
    gen_server:cast(?SERVER, {chat_kill, Session}).

% ------------------------------------------------------------------------------

init([]) ->
    Tid = ets:new(?TABLE, [set, protected, {keypos,2}]),
    {ok, #state{table=Tid}}.
    
    
handle_cast(status, State = #state{table=Tid}) ->
    internal_status(Tid),
    {noreply, State};
    
handle_cast({chat_kill, Session}, State = #state{table=Tid}) ->
    internal_kill(Session, Tid),
    {noreply, State};
    
handle_cast({chat_join, Session, ChannelName}, State = #state{table=Tid}) ->
    case validation:validate_chat_channel(ChannelName) of
        false ->
            Session:add_message(?ERROR_20001),
            {noreply, State};
        true -> 
            Channel = case ets:lookup(Tid, ChannelName) of
                [A] -> A;
                [] -> #channel{name=ChannelName}
            end,
            OldSessions = Channel#channel.sessions,
            
            case lists:member(Session, OldSessions) of
                true ->
                    % Session is already in that channel
                    Session:add_message(?ERROR_20002),
                    ok;
                false ->
                    NewSessions = OldSessions ++ [Session],
                    NewChannel = Channel#channel{sessions=NewSessions},
                    ets:insert(Tid, NewChannel),
                    
                    OtherNames = lists:map(fun(X) -> X:get_name() end, NewSessions),
                    
                    % Send the joiner the list of names
                    Session:add_message({chat_join_self, ChannelName, OtherNames}),
                    
                    % Send all other, the join
                    lists:foreach(fun(X) -> X:add_message({chat_join, ChannelName, Session:get_name()}) end, OldSessions)
            end,
            {noreply, State}
    end;

handle_cast({chat_send, Session, ChannelName, Message}, State = #state{table=Tid}) ->
    case validation:validate_chat_message(Message) of
        true ->
            case ets:lookup(Tid, ChannelName) of
                [Channel] ->
                    case lists:member(Session, Channel#channel.sessions) of
                        true ->
                            lists:foreach(
                                fun(UserSession) ->
                                    UserSession:add_message({chat_send, ChannelName, Session:get_name(), Message})
                                end,
                                Channel#channel.sessions
                            );
                        false ->
                            error_logger:error_msg("[CHAT] SessionÊ~p sending message to channel '~s' in which he is no member: ~p~n", [Session, ChannelName, Message])
                    end;
                [] ->
                    error_logger:error_msg("[CHAT] Session ~p sending message to non-existing channel '~s': ~p~n", [Session, ChannelName, Message])
            end;
        false ->
            Session:add_message(?ERROR_20003),
            ok
    end,
    {noreply, State};
    
handle_cast({chat_part, Session, ChannelName}, State = #state{table=Tid}) ->
    case ets:lookup(Tid, ChannelName) of
        [Channel] ->
            NewSessions = lists:filter(
                fun(UserSession) ->
                    not(UserSession =:= Session)
                end,
                Channel#channel.sessions
            ),
            ets:insert(Tid, Channel#channel{sessions=NewSessions}),
            
            lists:foreach(fun(S) ->
                S:add_message({chat_part, ChannelName, Session:get_name()}) end,
                NewSessions
            ),            
            Session:add_message({chat_part_self, ChannelName});
        [] ->
            error_logger:error_msg("[CHAT] Session ~p parting from a non-existing channel '~s'", [Session, ChannelName]),
            ok
    end,
    {noreply, State}.
    

handle_call(channel_list, _From, State = #state{table=Tid}) ->
    Channels = ets:tab2list(Tid),
    ChannelNames = lists:map(fun(X) -> X#channel.name end, Channels),
    {reply, {ok, ChannelNames}, State};
    
handle_call(_Message, _From, State) ->
    {noreply, State}.
    
handle_info(_Message, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% -------------------
% internal helper functions:


%%%
% Deletes the given session from all channels.
% The appropriate chat_part message gets send, too.
internal_kill(Session, Tid) ->
    internal_kill(Session, Tid, ets:first(Tid)).
    
internal_kill(_, _, '$end_of_table') -> ok;
internal_kill(Session, Tid, Element) ->
    [Channel] = ets:lookup(Tid, Element),
    
    OldSessions = Channel#channel.sessions,
    NewSessions = lists:filter(
        fun(X) ->
            not(X =:= Session)
        end,
        OldSessions
    ),
    
    lists:foreach(fun(S) -> S:add_message({chat_part, Channel#channel.name, Session:get_name()}) end, NewSessions),
    
    % overwrite the existing channel with the new one.
    ets:insert(Tid, Channel#channel{sessions=NewSessions}),
    
    Next = ets:next(Tid, Element),
    internal_kill(Session, Tid, Next).
    
    
%%
% Prints the status about the chat on the output.
internal_status(Tid) ->
    io:format("Channel\tSessions"),
    internal_status(Tid, ets:first(Tid)),
    io:format("-- end of channel status --~n").
    
internal_status(_, '$end_of_table') -> ok;
internal_status(Tid, Key) ->
    [Channel] = ets:lookup(Tid, Key),
    io:format("~s\t~w~n", [Channel#channel.name, length(Channel#channel.sessions)]),
    
    internal_status(Tid, ets:next(Tid, Key)).