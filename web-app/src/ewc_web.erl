%% @author Stephan Zeissler <zeisss@moinz.de>

%% @doc Web server for Erlmmo Web-App

-module(ewc_web).
-author('Stephan Zeissler <zeisss@moinz.de>').

-export([start/1, stop/0, loop/2]).


-define(CONTENT_TYPE, "text/html"). % TODO: Replace with application/json

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    
    io:format("~s ~s~n", [Method, Path]),
    
    
    case Path of
        "apiv1/" ++ ApiCallv1 ->
            % io:format("~s~n", [ApiCallv1]),
            case handle_apiv1_request(Method, ApiCallv1, Req) of
                {ok, Data} ->
                    send_result(Req, Data);
                {error, Code, Message} ->
                    send_error(Req, Code, Message);
                _ ->
                    send_error(Req, 500, "Internal error")
            end;
        _ ->
            Req:serve_file(Path, DocRoot)
    end.

%% Internal API

handle_apiv1_request('POST', "auth/login", Req) ->
    QS = Req:parse_post(),
    Username = proplists:get_value("username", QS),
    Password = proplists:get_value("password", QS),
    case session_service:login(Username, Password) of
        {ok, Sessionkey, Timeout} ->
            {ok, {struct, [{sessionkey, Sessionkey}, {timeout, Timeout}]}};
        _ ->
            {error, 600, "Unable to authenticate user."}
    end;

handle_apiv1_request('POST', "auth/logoff", Req) ->
    QS = Req:parse_post(),
    SessionKey = proplists:get_value("sessionkey", QS),
    SessionPid = session_service:find(SessionKey),
    
    case SessionPid of
        undefined -> {error, 001, "Invalid sessionkey."};
        _ ->
            case session_handler:logoff(SessionPid) of
                ok ->
                    {ok, 'OK'};
                _ ->
                    {error, 600, "Unable to logoff user"}
            end
    end;
    
handle_apiv1_request('POST', "chat/send", Req) ->
    QS = Req:parse_post(),
    Message = proplists:get_value("message", QS),
    SessionKey = proplists:get_value("sessionkey", QS),
    SessionPid = session_service:find(SessionKey),
    
    case SessionPid of
        undefined -> {error, 001, "Invalid sessionkey."};
        _ ->
            % Async cast
            ok = session_handler:chat_say(SessionPid, Message),
            {ok, 'OK'}
    end;
    
handle_apiv1_request('GET', "event/get", Req) ->
    QS = Req:parse_qs(),
    SessionKey = proplists:get_value("sessionkey", QS),
    SessionPid = session_service:find(SessionKey),
    
    case SessionPid of
        undefined -> {error, 001, "Invalid sessionkey."};
        _ ->
            Events = session_handler:event_get(SessionPid),
            {ok, Events}
    end;
        
handle_apiv1_request('GET', Call, _Req) when Call =:= "test"; Call =:= "test/test" ->
    {ok, 'OK'};
handle_apiv1_request(Method, ApiCall,_) when Method =:= 'GET'; Method =:= 'HEAD'; Method =:= 'POST' ->
    {error, 404, "Unknown api method: " ++ ApiCall};
handle_apiv1_request(_,_,_) ->
    {error, 2, "Invalid request"}.


send_error(Req, Code, Message) ->
    Req:respond({
        200,
        [{"Content-Type", ?CONTENT_TYPE}],
        mochijson2:encode(
            {
                struct,
                [
                    {"code", Code},
                    {"message", list_to_binary(Message)}                    
                ]
            }
        )
    }).
    
send_result(Req, Data) ->
    Req:respond({200, [{"Content-Type", ?CONTENT_TYPE}], mochijson2:encode(Data)}).

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
