-module(chat_channel).

-export([start/1, join/2, send/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-record(state, {ref, consumers=[]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% PUBLIC API
%%%% ==========
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(ChannelRef) ->
    gen_server:start_link(?MODULE, ChannelRef, []).
    
join(ChannelPid, ConsumerRef) ->
    gen_server:call(ChannelPid, {join, ConsumerRef}).
    
send(ChannelPid, ConsumerRef, Message) ->
    gen_server:cast(ChannelPid, {send, ConsumerRef, Message}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% CALLBACK API
%%%% ============
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Name) ->
    {ok, #state{ref=Name}}.

handle_call({join, ConsumerRef}, From, State) ->
    io:format("[#~w] Joined by: ~w~n", [State#state.ref, ConsumerRef]),
    
    NewConsumers = lists:append(State#state.consumers, [ConsumerRef]),
    NewState     = State#state{consumers=NewConsumers},
    
    % Send all (including new consumer) a 'join' message
    broadcast_message(
        {chat_join, State#state.ref, ConsumerRef},
        NewState
    ),
    
    {reply, ok, NewState};
    
handle_call(Message, From, State) ->
    io:format("[chat_server] Unknown message: ~w (call)~n", [Message]),
    {reply, ok, State}.
    
handle_cast({send, ConsumerRef, Message}, State) ->
    broadcast_message({chat_send, State#state.ref, ConsumerRef, Message}, State),
    {noreply, State};
    
handle_cast(Message, State) ->
    io:format("[chat_server] Unknown message: ~w (cast)~n", [Message]),
    {noreply, State}.
    
handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.
    
code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal helper functions (private)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
% Sends the given message to all consumers of this channel.
broadcast_message(Message, State) ->
    lists:foreach(fun(ConsumerRef) ->
        Consumer = chat_server:lookup_consumer(ConsumerRef),
        chat_server:send_consumer_message(Consumer, Message)
    end,
    State#state.consumers).