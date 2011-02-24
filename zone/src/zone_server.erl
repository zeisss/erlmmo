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

-export([start_link/1, join/3, part/2, add_action/4]).


%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start_link(ZoneId) ->
  gen_server:start_link({global, ZoneId}, ?MODULE, [ZoneId], []).
    
join(ZoneId, PlayerId, Options) ->
    gen_server:cast({global, ZoneId}, {zone_join, ZoneId, PlayerId, Options}).
    
part(ZoneId, PlayerId) ->
    gen_server:cast({global, ZoneId}, {zone_part, ZoneId, PlayerId}).
    
% Sends the action to move to X,Y to the zone
add_action(ZoneId, PlayerId, move, {X,Y}) ->
    gen_server:cast({global, ZoneId}, {zone_action, PlayerId, move, {X,Y}}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {
    zone_id,
    timer_ref,
    
    %%
    % actions = [Action]
    % Action = {Player, ActionName, Options}
    % Player = term()
    % ActionName = move
    % Option = {NewX, NewY}
    % NewX = NewY = int()
    %%
    actions=gb_trees:empty(),
    pos_tree=gb_trees:empty(),
    players=[],
    objects=gb_trees:empty()
}).

init([ZoneId]) ->
    % TODO: Downcrease this to 200ms? 
    Ref = timer:apply_interval(10000, erlang, send, [self(), new_tick]),
    State = #state{zone_id=ZoneId, timer_ref=Ref},
    {ok, State}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast({zone_join, ZoneId, PlayerId, Options}, State) ->
  {noreply, State};
handle_cast({zone_part, ZoneId, PlayerId}, State) ->
  {noreply, State};

%%
% Stores the new action in the 'actions'-tree
% We use the playerId + actionName as the key, since thus it
% automatically overwrites previous commands
handle_cast({zone_action, PlayerId, ActionName, Options}, State) ->
  NewActions = gb_trees:enter({PlayerId, ActionName}, Options, State#state.actions),
  NewState = State#state{actions=NewActions},
  {noreply, NewState}.


handle_info(new_tick, State) ->
  io:format("Tick! ~w~n", [State]),
  lists:foreach(
    fun({_Key = {PlayerId, ActionName},ÊVal}) ->
        io:format("~w performs ~w~n", [PlayerId, ActionName])
    end,
    gb_trees:to_list(State#state.actions)
  ),
  
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
