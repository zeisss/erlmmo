-module (skill_tree).

-export([get_skill/1, skill_groups/0, is_learnable/2, update/2, test/0]).

-include_lib("include/erlmmo.hrl").

% ------------------------------------------------------------------------------
% ATTRIBUTES
get_skill(intelligence) ->
    #skill{skillId = intelligence, name = <<"Intelligence">>, description = <<"Dummy Text (TODO)">>};
get_skill(strength) ->
    #skill{skillId=strength,    name = <<"Strength">>, description = <<"Dummy Text (TODO)">>};
get_skill(life) ->
    #skill{skillId=life,        name = <<"Life">>, description= <<"Dummy Text (TODO)">>};
get_skill(willpower) ->
    #skill{skillId=willpower,   name = <<"Willpower">>, description= <<"Dummy Text (TODO)">>};
get_skill(memory) ->
    #skill{skillId=memory,      name = <<"Memory">>, description= <<"Dummy Text (TODO)">>};
% ------------------------------------------------------------------------------
% FIGHTING
get_skill(short_fighting) ->
    #skill{skillId=short_fighting,  name = <<"Short-Range Fighting">>, description = <<"Dummy Text (TODO)">>};
get_skill(long_fighting) ->
    #skill{skillId=long_fighting,   name = <<"Long-Range Fighting">>, description = <<"Dummy Text (TODO)">>};
get_skill(scanning) ->
    #skill{skillId=scanning,        name = <<"Range Scanning">>, description = <<"Dummy Text (TODO)">>};
get_skill(advanced_scanning) ->
    #skill{skillId=advanced_scanning,
                   name = <<"Advanced Range Scanning">>,
                   description = <<"Dummy Text (TODO)">>,
                   requirement=[
                      #skill_level{skillId=scanning, level=5}
                   ]}.

%%%%
% Returns the definition 
skill_groups() ->
    [
        { #skill_group{groupId=base_skills, groupName = <<"Base Attributes">>}, [
            intelligence,
            strength,
            life,
            willpower,
            memory
        ]},
        
        { #skill_group{groupId=fight_skills, groupName = <<"Fight Attributes">>}, [
            short_fighting,
            long_fighting,
            scanning,
            advanved_scanning
        ]}
    ].
    
%%%%
%
update(SkillLevel, SkillList) ->
    update1(SkillLevel, SkillList, []).
    
update1(SkillLevel, [], Rest) -> [SkillLevel | Rest];
update1(SkillLevel = #skill_level{skillId=SkillId}, [#skill_level{skillId=SkillId}| SkillList], Rest) ->
    update1(SkillLevel, SkillList, Rest);
update1(SkillLevel, [SkillLevel2 | SkillList], Rest) ->
    update1(SkillLevel, SkillList, [SkillLevel2] ++ Rest).
    
    
%%%%
% Returns true if the given #skill_level{} is learnable
is_learnable(#skill_level{skillId=SkillId, level=Level}, KnownSkills) when is_list(KnownSkills), Level > 1 ->
    % Check if the previous level is alredy known
    Req = #skill_level{skillId=SkillId, level=Level - 1},
    lists:member(Req, KnownSkills);

is_learnable(#skill_level{skillId=TargetSkillId, level=1}, KnownSkills) when is_list(KnownSkills) ->
    % We want to learn a new skill, so check if we have all requirements
    TargetSkill = get_skill(TargetSkillId),
    
    % Filter out all requirements, that do not match
    Requirements = lists:filter(
        fun (_RequiredSkill = #skill_level{skillId=SkillId, level=Level}) ->
            ContainsRequiredSkill = lists:any(
                % True, when matching the required skill 
                fun(#skill_level{skillId=SkillId2, level=Level2}) when SkillId == SkillId2, Level =< Level2 -> true;
                    (_) -> false
                end,
                KnownSkills
            ),
            
            % False to filter the value out, when the skill of the requirement is filled
            not(ContainsRequiredSkill)
        end,
        TargetSkill#skill.requirement
    ),
    
    case Requirements of
        [] -> true;
        _ -> false
    end.
    
    
    
test() ->
    true = is_learnable(#skill_level{skillId=memory, level=1}, []),
    false= is_learnable(#skill_level{skillId=memory, level=2}, []),
    true = is_learnable(#skill_level{skillId=memory, level=2}, [
        #skill_level{skillId=memory, level=1}
    ]),
    
    false = is_learnable(#skill_level{skillId=advanced_scanning, level=1}, []),
    false = is_learnable(#skill_level{skillId=advanced_scanning, level=1}, [
        #skill_level{skillId=scanning, level=1}
    ]),
    false = is_learnable(#skill_level{skillId=advanced_scanning, level=1}, [
        #skill_level{skillId=scanning, level=4}
    ]),
    true  = is_learnable(#skill_level{skillId=advanced_scanning, level=1}, [
        #skill_level{skillId=scanning, level=5}
    ]),
    
    
    % Update:
    [#skill_level{skillId=memory, level=2}] = update(#skill_level{skillId=memory, level=2}, []),
    [#skill_level{skillId=memory, level=2}] = update(#skill_level{skillId=memory, level=2}, [#skill_level{skillId=memory, level=1}]),
    
    ok.
    
    
    
    
    