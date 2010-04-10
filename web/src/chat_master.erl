-module (chat_master).

-export([status/0, channel_list/0]).
-export([start_link/0, chat_join/2, chat_send/3, chat_part/2, chat_kill/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
            % TODO: Notify the user, that he cannot join that channel
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
                            error_logger:error_msg("SessionÊ~p sending message to channel '~s' in which he is no member: ~p~n", [Session, ChannelName, Message])
                    end;
                [] ->
                    error_logger:error_msg("Session ~p sending message to non-existing channel '~s': ~p~n", [Session, ChannelName, Message])
            end;
        false ->
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