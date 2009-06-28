-module(zone_loader).

-compile(export_all).

%%%
% Returns a structure holding info about the zone. See zone_server:init() how this is interpreted.
%
% load(Path, Filename) -> {Name, Width, Height, Fields}
% Filename = ZoneName = Name = Description = [char()]
% Width = Height = X = Y = Blocking int()
% Fields = [Field]
% Field = {X, Y, Name, Description, Blocking}
%
load ( Path, Filename ) ->
	case lists:reverse(Filename) of 
		"lre." ++ _ ->
			file:path_script([Path], Filename);
		_ ->
		   {"Demoforest", 3,1, [
				{0,0, "Demo 01", "A demo field", 0},
				{1,0, "Demo 02", "Another demo field.", 0},
				{2,0, "Demo 03", "And another one, but blocked :)", 1}
	       ]}
	end.