-module(chat_server).
%%
% The gen master server. It manages two ETS tables:
% - One containing all channels {ChannelRef, Pid}
% - The other containg all sessions: {SessionRef, [ChannelRef], CallbackFun}
% 
% The 'chat' module sends several messages for joining/parting.
% There are also methods for forwarding messages to a Ref.
% 
%%

% Public API
-export([start_link/0, destroy_channel/2]).

% Internal Helper API
-export([send_consumer_message/2, lookup_consumer/1, lookup_channel/1]).

% Callback API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).


%%%%
% consumer => ETS Table ID
% channel => ETS Table ID
-record(state, {consumer, channel}).

-record(consumer, {ref, opts = [], channelpids = []}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% PUBLIC API
%%%% ==========
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
% Start the gen_server.
start_link() ->
    gen_server:start_link({local, chat}, ?MODULE, [], []).
  
% Remove a channel(pid) from the internal table
destroy_channel(ChannelRef, ChannelPid) ->
    gen_server:call(chat, {destroy_channel, ChannelRef, ChannelPid}).
    
%%
% 
send_consumer_message(Consumer, Message) when record(Consumer, consumer) ->
    case proplists:get_value(callback, Consumer#consumer.opts) of
        undefined ->
            io:format("Unable to deliver message: ~w~n", [Message]),
            ok;
        CallbackFun ->
            CallbackFun({quit, client_already_connected})
    end.
    
% Returns the PID for the given ChannelRef, if started.
lookup_channel(ChannelRef) ->
    case ets:lookup(chat_channels, ChannelRef) of
        [] ->
            undefined;
        [{ChannelRef, Pid}] ->
            Pid
    end.

lookup_consumer(ConsumerRef) ->
    case ets:lookup(chat_consumers, ConsumerRef) of
        [] ->
            undefined;
        [Consumer] ->
            Consumer
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% CALLBACK API
%%%% ============
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    ConsumerTable = ets:new(chat_consumers, [set, protected, named_table, {keypos, 2}]), % {ConsumerRef, Options, [ChannelPid]}
    ChannelTable  = ets:new(chat_channels,  [set, protected, named_table]), % {ChannelAtom, ChannelPid}
    
    {ok, #state{consumer=ConsumerTable, channel=ChannelTable}}.

%%
%
handle_call(ping, From, State) ->
    {reply, pong, State};
    
handle_call({destroy_channel, ChannelRef, ChannelPid}, From, State) ->
    ets:delete(State#state.channel, ChannelRef),
    
    % NOTE:
    % This normally happens only, when a channel is empty
    % so there should be no need to iterate over all
    % consumers and remove their pid
    % But maybe we should do it nontheless?
    
    {reply, ok, State}.
    
%%
%
handle_cast({connect, ClientRef, Options}, State) when is_list(Options) ->
    % Check, if a client is already connected
    case ets:lookup(State#state.consumer, ClientRef) of
        % No consumer yet, so save it
        [] ->
            io:format("[chat_server] ~w connected.~n", [ClientRef]),
            ets:insert(State#state.consumer, #consumer{ref=ClientRef, opts=Options});
        
        % There is already a consumer, so send the CallbackFun
        % the quit message
        [Consumer] ->
            send_consumer_message(Consumer, {quit, client_already_connected})
    end,
    {noreply, State};

handle_cast({join, ConsumerRef, ChannelRef, Options}, State) ->
    io:format("[chat_server] ~w join #~w (~w)~n", [ConsumerRef, ChannelRef, Options]),
    
    % Store the ChannelPid in the consumer
    Consumer = lookup_consumer(ConsumerRef),
    
    case Consumer of
        undefined ->
            error_logger:error_report([
                {type, consumer_not_found},
                {consumerref, ConsumerRef},
                {channelref, ChannelRef},
                {options, Options}
            ]),
            % We abort here
            {noreply, State};
            
        _ ->
            % Lookup the PID
            ChannelPid = case lookup_channel(ChannelRef) of
                % New channel
                undefined ->
                    {ok, Pid} = chat_channel:start(ChannelRef),
                    ets:insert(State#state.channel, {ChannelRef, Pid}),
                    Pid;
                 
                % Channel already exists
                Pid when is_pid(Pid) ->
                    Pid
            end,
            
            % Add the channel to the channel list of the consumer
            NewChannelPids = lists:append(Consumer#consumer.channelpids, [ChannelPid]),
            NewConsumer = Consumer#consumer{channelpids=NewChannelPids},
            ets:insert(State#state.consumer, NewConsumer),
            
            % Notify the channel
            chat_channel:join(ChannelPid, ConsumerRef),
            
            {noreply, State}
    end;

handle_cast({part, ConsumerRef, ChannelRef, Options}, State) ->
    Consumer = lookup_consumer(ConsumerRef),
    
    case Consumer of
        undefined ->
            error_logger:error_report([
                {type, consumer_not_found},
                {consumerref, ConsumerRef},
                {channelref, ChannelRef},
                {options, Options}
            ]);
        _ ->
            % Check the channel now
            ChannelPid = lookup_channel(ChannelRef),
            
            case ChannelPid of
                undefined ->
                    error_logger:error_report([
                        {type, channel_not_found},
                        {consumerref, ConsumerRef},
                        {channelref, ChannelRef},
                        {options, Options}
                    ]);
                _ ->
                
                    % Ok, channel and consumer is fine
                    
                    % 1) notify the chat channel
                    chat_channel:part(ChannelPid, ConsumerRef, Options),
                    
                    % 2) remove the channel from the consumer
                    NewChannels = lists:delete(ChannelPid, Consumer#consumer.channelpids),
                    NewConsumer = Consumer#consumer{channelpids=NewChannels},
                    ets:insert(State#state.consumer, NewConsumer),
                    
                    ok
            end            
    end,

    % We abort here
    {noreply, State};
    
handle_cast(Request, State) ->
    io:format("[chat_server] Unknown message: ~w (cast)~n", [Request]),
    
    {noreply, State}.
    
handle_info(Info, State) ->
    io:format("~w~n", [Info]),
    {noreply, State}.
    
    
terminate(Reason, State) ->
    ok.
    
code_change(OldVsn, State, Extra) ->
    {ok, State}.
    
% format_status(Opt, [PDict, State]) -> state.
    

