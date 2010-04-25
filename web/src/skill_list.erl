-module (skill_list).

% Create/Read
-export([new/0, get_level/2, get_points_per_second/2]).

% Write
-export([update_skillpoints/4, get_missing_skillpoints/2]).

-include_lib("include/erlmmo.hrl").

%%%
% list = [{SkillId, LearnedLevel, LearnedPoints}]
%     LearnedLevel  = The level that this skill has currently learned (0-5, 5 = max)
%     LearnedPoints = The points already learned to the next level (see skill_tree to fetch how much points are required for the next level)


new() ->
    gb_trees:empty().
    
%%%%
% Returns the current level of the given skill
get_level(SkillId, SkillList) ->
    case gb_trees:lookup(SkillId, SkillList) of
        none -> 0;
        {value, {Level, _Points}} -> Level
    end.


  
%%
% Updates the points for the given skill. If enough points for the next level are reached,
% the level will be updated and the 
%
update_skillpoints(SkillId, NewSkillPoints, Fun, Tree) ->
    {Level, Points} = case gb_trees:lookup(SkillId, Tree) of
        none ->
            {0,0};
        {value, A} ->
            A
    end,
    
    
    Skill = skill_tree:get_skill(SkillId),
    RequiredSkillPoints = skill_tree:get_required_skillpoints(Level + 1, Skill#skill.level),
    
    % If current points + new points > required points
    % io:format("~p  >= ~p - ~p~n", [NewSkillPoints, RequiredSkillPoints, Points]),
            
    case NewSkillPoints >= RequiredSkillPoints - Points of
        true ->
            Fun(SkillId, Level + 1),
            gb_trees:enter(SkillId, {Level + 1, 0}, Tree);
        false ->
            gb_trees:enter(SkillId, {Level, Points + NewSkillPoints}, Tree)
    end.
    
%%%%%%
%
% Returns undefined when no more levels are available for the given skill (maximum_reached).
get_missing_skillpoints(SkillId, SkillList) ->
    Skill = skill_tree:get_skill(SkillId),
    
    {CurrentLevel, ActPoints} = case gb_trees:lookup(SkillId, SkillList) of
        none ->
            {0, 0};
        {value, {Level, Points}} ->
            {Level, Points}
    end,
    
    case skill_tree:get_required_skillpoints(CurrentLevel + 1, Skill#skill.level) of
        undefined ->
            undefined;
        RequiredPoints ->
            RequiredPoints - ActPoints
    end.
    
%%%
% Returns how many skillpoints a player should get per seconds. 
get_points_per_second(SkillId, SkillList) ->
    Skill = skill_tree:get_skill(SkillId),
    
    Primary  = get_level(Skill#skill.primary,   SkillList),
    Secondary= get_level(Skill#skill.secondary, SkillList),
    
    Learning = get_level(learning, SkillList),
    
    % Eve online:
    % (primary attribute + secondary attribute/2) x (1 + 0.02 x Learning skill level)
    Points = (Primary + (Secondary/2)) * (1 + 0.02 * Learning ),
    case Points of
        _ when Points < 1 ->
            1;
        _ ->
            Points
    end.
    