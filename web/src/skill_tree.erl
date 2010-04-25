-module (skill_tree).

-export([get_skill/1, skill_groups/0, get_required_skillpoints/2]).

-include_lib("include/erlmmo.hrl").

% ------------------------------------------------------------------------------
% ATTRIBUTES
get_skill(intelligence) ->
    #skill{skillId = intelligence, name = <<"Intelligence">>, description = <<"Dummy Text (TODO)">>,
           primary=intelligence, secondary=strength, level = 1};
get_skill(strength) ->
    #skill{skillId=strength,    name = <<"Strength">>, description = <<"Dummy Text (TODO)">>,
           primary=strength, secondary=perception, level = 1};
get_skill(perception) ->
    #skill{skillId=perception,        name = <<"Perception">>, description= <<"Dummy Text (TODO)">>,
           primary=perception, secondary=charisma, level = 1};
get_skill(charisma) ->
    #skill{skillId=charisma,   name = <<"Charisma">>, description= <<"Dummy Text (TODO)">>, 
            primary=charisma, secondary=memory, level = 1};
get_skill(memory) ->
    #skill{skillId=memory,      name = <<"Memory">>, description= <<"Dummy Text (TODO)">>,
           primary=memory, secondary=intelligence, level = 1};
% ------------------------------------------------------------------------------
% LEARNING
get_skill(learning) ->
    #skill{skillId=learning,      name = <<"Learning">>, description= <<"Dummy Text (TODO)">>, 
           primary=intelligence, secondary=memory, level = 1};
get_skill(adv_learning) ->
    #skill{skillId=adv_learning,  name = <<"Advanced Learning">>, description= <<"Dummy Text (TODO)">>,
            level = 3, requirement=[learning],
            primary=intelligence, secondary=memory};
% ------------------------------------------------------------------------------
% FIGHTING
get_skill(short_fighting) ->
    #skill{skillId=short_fighting,  name = <<"Short-Range Fighting">>, description = <<"Dummy Text (TODO)">>,
           primary=intelligence, secondary=memory, level = 1};
get_skill(long_fighting) ->
    #skill{skillId=long_fighting,   name = <<"Long-Range Fighting">>, description = <<"Dummy Text (TODO)">>,
           primary=intelligence, secondary=memory, level = 2};
get_skill(scanning) ->
    #skill{skillId=scanning,        name = <<"Range Scanning">>, description = <<"Dummy Text (TODO)">>,
           primary=intelligence, secondary=memory, level = 1};
get_skill(advanced_scanning) ->
    #skill{skillId=advanced_scanning,
            name = <<"Advanced Range Scanning">>,
            description = <<"Dummy Text (TODO)">>,
            level = 2,
            primary=intelligence, secondary=memory,
            requirement=[scanning]}.
% ------------------------------------------------------------------------------

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
% ------------------------------------------------------------------------------

%%%
% Returns the skillpoints required to reach the next level.
get_required_skillpoints(SkillLevelType, ActualSkillLevel) when ActualSkillLevel < 6->
    Points = case ActualSkillLevel of
        1 -> 250;
        2 -> 414;
        3 -> 8000;
        4 -> 45255;
        5 -> 256000
    end,
    Points * SkillLevelType; % A Level II Skill requires 2x the time and so on
get_required_skillpoints(_SkillLevelType, _ActualSkillLevel) ->
    undefined.
