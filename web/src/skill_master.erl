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

-export([start_link/0, session_login/1, session_logout/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}).

-define(SKILLS_KEY(Session), {session, Session, skills}).
-define(LEARNING_KEY(Session), {session, Session, skill_learning}).

-include_lib("include/erlmmo.hrl").

-record(state, {
    tree
}).

% The session_skill is sotred in the #state.tree for every logged in sesion
-record(session_skill, {
    timer_ref=false,  % The timer, which notifies the skill_master when a skill is done
    skill_list=[],    % The list of skills the session currently can.
    skill_level=false % The skill_level currently in progress
}).

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).
    
%%%%
% Starts the session by loading it from the storage.
session_login(Session) ->
    gen_server:cast(?SERVER, {session_login, Session}).
    
%%%%
% Ends the session and stores it back in the storage.
session_logout(Session) ->
    gen_server:cast(?SERVER, {session_logout, Session}).
    
%%%%
%

    

% ---------------------------------------------------------------------------- %
    
    
init([]) ->
    State = #state{
        tree = gb_trees:empty()
    },
    {ok, State}.
    
handle_call(_Message, _From, State) ->
    {noreply, State}.
    
% Handle a session login
%
% 1) Load the learned skills for an account
% 2) Load the current skill for an account
% 3) Check if the current should be finished, while the user is offline
% 4)  Schedule the current skill, if not finished
% 5) 
handle_cast({session_login, Session}, State = #state{tree=Tree}) ->
    {ok, LearnedSkills} = storage:load_object(?SKILLS_KEY(Session)),
    SessionSkill = #session_skill {
        skill_list = LearnedSkills
    },
    
    {ok, Learning} = storage:load_objects(?LEARNING_KEY(Session)),
    
    NewSessionSkill = case Learning of
        none -> SessionSkill;
        {SkillLevel = #skill_level{}} ->
            TimerRef = timer:send_integerval(timer:minutes(1), {skill_done, Session}),
            
            SessionSkill#session_skill {
                skill_level = SkillLevel,
                timer_ref = TimerRef
            }
    end,
    
    
    {noreply, State#state{tree=gb_trees:enter(Session, NewSessionSkill, Tree)}};
    
handle_cast({session_logout, Session}, State = #state{tree=Tree}) ->
    %SessionSkill = gb_trees:lookup(Session, Tree),
    
    % Store the skill_list
    %NewTree = case SessionSkill#session_skill.skill_list of
%        none ->
%            Tree;
%        _ ->
%            storage:save_object(?SKILLS_KEY(Session),SessionSkill#session_skill.skill_list),
%            gb_trees:delete(Session, Tree)
%    end,
%  {noreply, State#state{tree=NewTree}}.
   {noreply, State}. 
    

    
handle_info({skill_done, Session}, State = #state{tree=Tree}) ->
    % Clear the Element from the tree
    SessionSkill = gb_trees:lookup(Session, Tree),
    NewSessionSkill = SessionSkill#session_skill {
        skill_level = false,
        timer_ref = false,
        skill_list = skill_tree:update(SessionSkill#session_skill.skill_level, SessionSkill#session_skill.skill_list)
    },
    NewTree = gb_trees:update(Session, NewSessionSkill, Tree),
    
    {noreply, State#state{tree=NewTree} }.
    
terminate(_Reason, _State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
% ---------------------------------------------------------------------------- %

    