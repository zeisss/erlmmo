-module (couchdb).

-export([request/3, start_request/3, end_request/1, start/4]).

request(Server, Port, Database) ->
  {ok, Pid} = start_request(Server, Port, Database),
  end_request(Pid).
  
start_request(Server, Port, Database) ->
    {ok, spawn_link(?MODULE, start, [self(), Server, Port, Database])}.
    
end_request(Pid) ->
    receive 
        {couchdb_data, Pid, Data, Body} -> {ok, Data, Body};
        {couchdb_error, Pid, Reason} -> {error, Reason}
    end.
    

start(Pid, Server, Port, Database) ->
    case internal_request(Server, Port, Database, "GET") of
        {ok, Data, Body} ->
            Pid ! {couchdb_data, self(), Data, Body};
        {error, Reason} ->
            Pid ! {couchdb_error, self(), Reason}
    end.
    
internal_request(Server, Port, Database, Request) ->
  {ok, Sock} = gen_tcp:connect(Server, Port, [list, {packet, http}, {active, true}]),
  
  gen_tcp:send(Sock, Request ++ " " ++ Database ++ " HTTP/1.1" ++ [10]),
  gen_tcp:send(Sock, "Host: " ++ Server ++ [10]),
%  gen_tcp:send(Sock, "Keep-Alive: 1" ++ [10]),
%  gen_tcp:send(Sock, ")
  gen_tcp:send(Sock, [10]),
  
  loop(Sock, [], "").
  
loop(Sock, HttpData, Body) ->
    receive 
        {http_response, Sock, {1,1}, 200, "OK"} -> 
           loop(Sock, [], Body);
           
        {http_header, Sock, _, HeaderName, _, HeaderValue} ->
            io:format("~p: ~p~n", [HeaderName, HeaderValue]),
            loop(Sock, HttpData ++ [{HeaderName, HeaderValue}], Body);
            
        {http_eoh, Sock} ->
            io:format("eoh~n"),
            loop(Sock, HttpData, Body);
           
        {http_error, Sock, BodyData} ->
            io:format(": ~p~n", [BodyData]),
            loop(Sock, HttpData, Body ++ BodyData);
        
        {tcp_closed, Sock} ->
            {ok, HttpData, Body};    
        {error, closed} ->
            {ok, HttpData, Body}
%        ;A ->
%            io:format("~p", [A]),
%            {error, unknown_data}
        
        after 5000 -> {error, timeout}  
    end.