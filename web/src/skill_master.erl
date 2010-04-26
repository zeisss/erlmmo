%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module provides the skill-tree feature for the players.
% Similar to the one in eve-online.
%
% Messages:
% - {skill_finished, ...}
% - {skill_status, ...}  
% 
% Implementation Note:
% This module is implemented as a gen_server which runs a timer for each session.
% When a session gets started, it calls start_session(). The gen_server loads
% the skill data for the player and calculates any progress which happened
% while offline.
% Afterwards, a timer gets started, to notify the gen_server when the skill
% got finished. When a skill finishs, it notifies the Session with a {skill_finished}
% message.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module (skill_master).

-export([start_link/0, test/0]).

-export([session_login/1, session_logout/1, pause_skill/1, schedule_skill/2, get_skilllist/1, get_current_skill/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).

-define(SKILLS_KEY(Session), {session, Session, skills}).
-define(LEARNING_KEY(Session), {session, Session, skill_learning}).

-include_lib("include/erlmmo.hrl").

%%
% Starts the gen_server backend for the other interface functions.
start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).
    
%%
% Starts the session by loading it from the storage.
session_login(Session) ->
    gen_server:cast(?SERVER, {session_login, Session}).
    
%%
% Ends the session and stores it back in the storage.
session_logout(Session) ->
    gen_server:cast(?SERVER, {session_logout, Session}).

%%
% Pauses the current skill and updates the skillpoints in the underlying skill_list.
pause_skill(Session) ->
    gen_server:cast(?SERVER, {pause_skill, Session}).

%%
% Starts/Continues learning the given skill.
schedule_skill(Session, SkillId) ->
    gen_server:cast(?SERVER, {schedule_skill, Session, SkillId}).
    
get_skilllist(Session) ->
    gen_server:call(?SERVER, {get_skilllist, Session}).

get_current_skill(Session) ->
    gen_server:call(?SERVER, {get_current_skill, Session}).
% ---------------------------------------------------------------------------- %
    
-record(state, {
    tree
}).

% The session_skill is stored in the #state.tree for every logged in sesion
-record(session_skill, {
    timer_ref = undefined,  % The timer, which notifies the skill_master when a skill is done
    skill_list=[],            % The list of skills the session currently can.
    start_time = undefined,       % erlang:now() when the skill was last started.
    current_skill = undefined    % The SkillId currently learning
}).

    
init([]) ->
    State = #state{
        tree = gb_trees:empty()
    },
    {ok, State}.
    
handle_call({get_current_skill, Session}, _From, State = #state{tree=Tree}) ->
    case gb_trees:lookup(Session, Tree) of
        {value, #session_skill{current_skill=SkillId}} ->
            {reply, {ok, SkillId}, State};
        none ->
            {reply, {error, not_loggedin}, State}
    end;
handle_call({get_skilllist, Session}, _From, State = #state{tree=Tree}) ->
    case gb_trees:lookup(Session, Tree) of
        {value, #session_skill{skill_list=SkillList}} ->
            {reply, {ok, SkillList}, State};
        none ->
            {reply, {error, not_loggedin}, State}
    end.
    
% Handle a session login
%
% 1) Load the learned skills for an account
% 2) Load the current skill for an account
% 3) Check if the current should be finished, while the user is offline
% 4)  Schedule the current skill, if not finished
% 5) 
handle_cast({session_login, Session}, State = #state{tree=Tree}) ->
    SkillList = case storage:load_object(?SKILLS_KEY(Session)) of
        {ok, undefined} ->
            skill_list:new();
        {ok, List} ->
            List
    end,
    
    SessionSkill = case storage:load_object(?LEARNING_KEY(Session)) of
        {ok, undefined} ->
            #session_skill{
                skill_list = SkillList
            };
        {ok, {SkillId, Timestamp}} -> % We started SkillId at TimeStamp
            % Async Call which later starts the skill learning
            schedule_skill(Session, SkillId),
            
            % The call to update_*() calculates the points while beeing offline.
            update_skilllist(Session, #session_skill{
                skill_list = SkillList,
                current_skill=SkillId,
                start_time = Timestamp
            })
            
    end,
    
    {noreply, State#state{tree=gb_trees:enter(Session, SessionSkill, Tree)}};
    
handle_cast({session_logout, Session}, State = #state{tree=Tree}) ->
    NewTree = case gb_trees:lookup(Session, Tree) of
        none ->
            error_logger:error_msg("[SKILLMASTER] Ooops, can't find skill data for session."),
            Tree;
        {value, SessionSkill} ->
            % This stops the timer
            UpdatedSessionSkill = update_skilllist(Session, SessionSkill), 
            
            storage:save_object(?SKILLS_KEY(Session), UpdatedSessionSkill#session_skill.skill_list),
            
            case SessionSkill#session_skill.current_skill of
                undefined ->
                    storage:save_object(?LEARNING_KEY(Session), undefined);
                _ ->
                    storage:save_object(?LEARNING_KEY(Session),
                                        {SessionSkill#session_skill.current_skill,
                                         erlang:now()})
            end,
            
            gb_trees:delete(Session, Tree)
    end,
            
    {noreply, State#state{tree=NewTree}};
    
handle_cast({pause_skill, Session}, State = #state{tree=Tree}) ->
    {value, SessionSkill} = gb_trees:lookup(Session, Tree),
    
    % The new SessionSkill is cleared on all timer/learning references
    NewSessionSkill = update_skilllist(Session, SessionSkill),
    
    {noreply, State#state{tree=gb_trees:enter(Session, NewSessionSkill, Tree)}};
    
    
handle_cast({schedule_skill, Session, SkillId}, State = #state{tree=Tree}) ->
    {value, SessionSkill} = gb_trees:lookup(Session, Tree),
    
    case SessionSkill#session_skill.current_skill of
        undefined ->
            Time = get_time(SkillId, SessionSkill#session_skill.skill_list),
            io:format("Schedulung skill ~p for ~p in ~w milliseconds.~n", [SkillId, Session, Time]),
            {ok, TRef} = timer:send_after(Time, {skill_done, Session}),
            
            NewSessionSkill = SessionSkill#session_skill{
                timer_ref = TRef,
                current_skill = SkillId,
                start_time = erlang:now()
            },
            NewTree = gb_trees:enter(Session, NewSessionSkill, Tree),
            {noreply, State#state{tree=NewTree}};
        _ ->
            {noreply, State}
    end.
    

handle_info({skill_done, Session}, State = #state{tree=Tree}) ->
    % Clear the Element from the tree
    {value, SessionSkill} = gb_trees:lookup(Session, Tree),
    
    NewSessionSkill = update_skilllist(Session, SessionSkill),
    
    {noreply, State#state{tree=gb_trees:enter(Session, NewSessionSkill, Tree)}}.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
% ---------------------------------------------------------------------------- %

%%%
% Updates the SessionSkill object to contain all earned skillpoints until now.
%
update_skilllist(Session, SessionSkill = #session_skill{current_skill=SkillId, skill_list=SkillList}) ->
    case SessionSkill#session_skill.current_skill of
        undefined ->
            SessionSkill;
        SkillId ->
            % Do not check the result, since timer_ref might (session_login) be undefined and this would raise an error
            timer:cancel(SessionSkill#session_skill.timer_ref),
            
            TimeSince = timer:now_diff(erlang:now(), SessionSkill#session_skill.start_time) / (1000 * 1000),
            
            EarnedSkillPoints = TimeSince * skill_list:get_points_per_second(SkillId, SkillList),
            
            io:format("Giving ~p ~p skillpoints~n", [Session:get_name(), EarnedSkillPoints]),
            
            NewSkillList = skill_list:update_skillpoints(
                SkillId,
                EarnedSkillPoints,
                fun(NewSkillId, NewLevel) ->
                    Session:add_message({skill_done, NewSkillId, NewLevel})
                end,
                SkillList
            ),
            
            NSK = SessionSkill#session_skill{
                timer_ref=undefined,
                start_time=undefined,
                current_skill=undefined,
                skill_list=NewSkillList
            },
            
            storage:save_object(?SKILLS_KEY(Session), NSK),
            storage:save_object(?LEARNING_KEY(Session), undefined),
            
            NSK
    end.
    
%%
% Returns the milliseconds required to finish this skill.
get_time(SkillId, SkillList) ->
    Missing = skill_list:get_missing_skillpoints(SkillId, SkillList),
    PointsPerSecond = skill_list:get_points_per_second(SkillId, SkillList),
    
    
    (trunc(Missing / PointsPerSecond)+1) * 1000.


test() ->
    ok = test1().
    
test1() ->
    S = session:new(<<"Miro">>,<<"Miro">>),
    {ok, _Pid} = skill_master:start_link(),
    skill_master:session_login(S),
    skill_master:schedule_skill(S, memory),
    
    {ok, memory} = get_current_skill(S),
    ok.
    