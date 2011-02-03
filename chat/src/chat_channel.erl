-module(chat_channel).

-export([start/1, join/2, send/3, part/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-record(state, {ref, consumers=[]}).

-define(TIMEOUT, 5000).

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

part(ChannelPid, ConsumerRef, Options) ->
    gen_server:call(ChannelPid, {part, part, ConsumerRef, Options}).

% Equal to part()
quit(ChannelPid, ConsumerRef, Options) ->
    gen_server:call(ChannelPid, {part, quit, ConsumerRef, Options}).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% CALLBACK API
%%%% ============
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Name) ->
    {ok, #state{ref=Name}}.

handle_call({join, ConsumerRef}, _From, State) ->
    io:format("[#~w] Joined by: ~w~n", [State#state.ref, ConsumerRef]),
    
    % Construct the new State
    NewConsumers = lists:append(State#state.consumers, [ConsumerRef]),
    NewState     = State#state{consumers=NewConsumers},
    
    % Notify the consumer that he has joined
    single_message({chat_channel_join, State#state.ref, State#state.consumers}, ConsumerRef),
    
    % Send all old consumers a 'join' message
    broadcast_message(
        {chat_join, State#state.ref, ConsumerRef},
        State
    ),
    
    {reply, ok, NewState};
 
%%%   
% PartType = quit |Êpart
%
handle_call({part, PartType, ConsumerRef, Options}, _From, State) ->
    io:format("[#~w] ~w parted.~n", [State#state.ref, ConsumerRef]),
    
    % Broadcast 'part'
    case PartType of
        part ->
            broadcast_message(
                {chat_part, State#state.ref, ConsumerRef, proplists:get_value(reason, Options, no_reason) },
                State
            );
        quit ->
            broadcast_message(
                {chat_quit, State#state.ref, ConsumerRef, proplists:get_value(reason, Options, no_reason) },
                State
            )        
    end,
    
    % Update the internal list
    NewConsumers = lists:delete(ConsumerRef, State#state.consumers),
    NewState = State#state{consumers=NewConsumers},
    
    case NewConsumers of
        [] ->
            {reply, ok, NewState, ?TIMEOUT}; % 'timeout' after 5 seconds
        _ ->
            {reply, ok, NewState}
    end;
    
handle_call(Message, _From, State) ->
    error_logger:error_report([
        {error, unknown_call},
        {channelref, State#state.ref},
        {consumers, State#state.consumers},
        {message, Message}        
    ]),
    {reply, ok, State}.
    
handle_cast({send, ConsumerRef, Message}, State) ->
    io:format("[#~w] ~w: ~w~n", [State#state.ref, ConsumerRef, Message]),
    broadcast_message({chat_send, State#state.ref, ConsumerRef, Message}, State),
    {noreply, State};
    
handle_cast(Message, State) ->
    error_logger:error_report([
        {error, unknown_cast},
        {channelref, State#state.ref},
        {consumers, State#state.consumers},
        {message, Message}        
    ]),
    {noreply, State}.
    
handle_info(timeout, State) ->
    io:format("Info: timeout!~n", []),
    
    % If a timeout happens, we want to quit
    {stop, timeout, State};
    
handle_info(Info, State) ->
    % io:format("[#~w] Unknown message: ~w (info)~n", [State#state.ref, Info]),
    error_logger:error_report([
        {error, unknown_info},
        {channelref, State#state.ref},
        {consumers, State#state.consumers},
        {info, Info}        
    ]),
    {noreply, State}.

terminate(_Reason, State) ->
    io:format("Timeout. Closing channel ~w ~n", [State#state.ref]),
    chat_server:destroy_channel(State#state.ref, self()),
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal helper functions (private)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_message(Message, ConsumerRef) ->
    io:format("~w: ~w ~n", [ConsumerRef, Message]),
    
    Consumer = chat_server:lookup_consumer(ConsumerRef),
    chat_server:send_consumer_message(Consumer, Message).
    
%%
% Sends the given message to all consumers of this channel.
broadcast_message(Message, State) ->
    lists:foreach(fun(ConsumerRef) ->
        single_message(Message, ConsumerRef)
    end,
    State#state.consumers).