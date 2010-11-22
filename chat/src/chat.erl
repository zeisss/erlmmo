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
% ClientRef = term()
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
join(ClientRef, ChannelRef, Options) ->
    gen_server:cast(?SERVER, {join, ClientRef, ChannelRef, Options}).
    

send(ClientRef, ChannelRef, Message) ->
    % NOTE: Parameter order switched
    chat_server:send(ChannelRef, ClientRef, Message).
    
part(ClientRef, ChannelRef, Options) ->
    gen_server:cast(?SERVER, {part, ClientRef, ChannelRef, Options}).