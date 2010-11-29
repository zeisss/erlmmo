-module(chat).

-compile(export_all).

%%%
%
% Callback Messages
% =================
% {quit, ErrorReason} -> Sent, when the client gots disconnected (e.g. normal or on connect)
%   
%   
%%%
-define(SERVER, chat).

%%%
% 
% 
start() ->
    application:start(chat).

%%%
% 
% 
stop() ->
    application:stop(chat).
    
    
available() ->
    case gen_server:call(?SERVER, ping) of
        pong -> true;
        _ -> false
    end.

%%%
% Adds a Client to the chat-system.
%
% connect(ClientRef, CallbackFun) -> ok
% ConsumerRef = term()
% Options = [Options]
% Options = {Key, Value}
%
% Key = callback, Value = Fun()
%%
connect(ClientRef, Options) ->
    gen_server:cast(?SERVER, {connect, ClientRef, Options}).

%%%
%
% OptionKey = reason, OptionValue = binary()
%%%
join(ConsumerRef, ChannelRef, Options) ->
    gen_server:call(?SERVER, {join, ConsumerRef, ChannelRef, Options}).
    

send(ConsumerRef, ChannelRef, Message) ->
    % NOTE: Parameter order switched
    case chat_server:lookup_channel(ChannelRef) of
        undefined ->
            {error, unknown_channel};
        Pid when is_pid(Pid) ->
            chat_channel:send(Pid, ConsumerRef, Message)
    end.
    
part(ConsumerRef, ChannelRef, Options) ->
    gen_server:call(?SERVER, {part, ConsumerRef, ChannelRef, Options}).
    
%%
% Send a message directly to the receiving consumer.
%
% whisper(S, R, M) -> Result
% S = R = consumerRef()
% consumerRef() = term()
% M = term()
% Result = ok |Ê{error, unknown_sender} |Ê{error, unknown_receiver}
whisper(SenderRef, ReceiverRef, Message) ->
    io:format("~w whispers ~w: ~w~n", [SenderRef, ReceiverRef, Message]),
    
    case chat_server:lookup_consumer(SenderRef) of
        undefined ->
            {error, unknown_sender};
        _ ->
            case chat_server:lookup_consumer(ReceiverRef) of
                undefined ->
                    {error, unknown_receiver};
                ReceiverConsumer ->
                    chat_server:send_consumer_message(ReceiverConsumer, {chat_whisper, SenderRef, Message}),
                    ok
            end
    end.
    
disconnect(ConsumerRef, Options) ->
    gen_server:call(?SERVER, {disconnect, ConsumerRef, Options}).