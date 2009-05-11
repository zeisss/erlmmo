-module(player_server).
-define(SERVER, ?MODULE).
-behaviour(gen_server).

-export([start_link/1, whereami/1, move/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, 
         handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ===================================================================
%% The user module works as a proxy between the web requests and the
%% central game processes. Multiple requests from the web end up here
%% and gets processes. 
%%   
%%                        /------\
%%  Browser ---> WebRequest --> Player --> Room
%%                        \-----/
%%
%% The user process gets started by after a successfull login and
%% holds the inventory, the current room and all the session stuff
%% in its internal state.
%
%% Functions for the WebRequest:
%%
%% move(n|e|w|s|se|sw|ne|nw) -> {ok, Name, Description, [Actions], [Objects]}
%% whereami() -> {X,Y}
%% logoff() -> ok
%% 
%% ===================================================================
-record(user_state, {
   user_id,
   inventory,
   area_id,
   position
 }).

%%====================================================================
%% API
%%====================================================================
start_link(UserId) ->
  Args = [UserId],
  Opts = [],
  %TODO: Remove the local name, as we will have multiple user_servers
  gen_server:start_link(?MODULE, Args, Opts).

whereami(Pid) ->
  gen_server:call(Pid, whereami).

move(Pid, Direction) ->
  gen_server:call(Pid, {move,Direction}).
  
%%====================================================================
%% CALLBACKs: gen_server
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server with its initial state.
%%--------------------------------------------------------------------
init([UserId]) ->
  % TODO: Load userdata by the given id
  State = #user_state{user_id=UserId, inventory=[], area_id="home", position={0,0}},
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages (sync)
%%--------------------------------------------------------------------
handle_call({move, Direction}, _From, State) ->
  {PosX, PosY} = State#user_state.position,
  NewPos = case Direction of 
    n -> {PosX-1, PosY};
    s -> {PosX+1, PosY};
    e -> {PosX, PosY +1};
    w -> {PosX, PosY -1};
    se -> {PosX+1, PosY+1};
    sw -> {PosX+1, PosY-1};
    nw -> {PosX-1, PosY-1};
    ne -> {PosX-1, PosY+1};
    _ -> {PosX, PosY}
  end,
  
  % TODO: Call the area server instead holding of the position here
  
  NewState = State#user_state{position=NewPos},
  Reply = {ok, "Unnamed", "No desc", [n,s,e,w,nw,ne,sw,se], []},
  {reply, Reply, NewState};
    
handle_call(whereami, _From, State) ->
  Reply = State#user_state.position,
  {reply, Reply, State};
  
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages (async)
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->

  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

