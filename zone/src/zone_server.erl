%%%%
% A zone_server represents a actual zone, where players can move around and do stuff.
% 
%%%%
-module(zone_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, join/3, part/2]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link(ZoneId) ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [ZoneId], []).
    
join(ZoneId, PlayerId, Options) ->
    gen_server:cast({global, ZoneId}, {zone_join, ZoneId, PlayerId, Options}).
    
part(ZoneId, PlayerId) ->
    gen_server:cast({global, ZoneId}, {zone_part, ZoneId, PlayerId}).
    



%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


init([ZoneId]) ->
    State = {ZoneId, []},
    
    {ok, State}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast({zone_join, ZoneId, PlayerId, Options}, State) ->
  {noreply, State};
handle_cast({zone_part, ZoneId, PlayerId}, State) ->
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
