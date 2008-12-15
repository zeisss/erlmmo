-module(area).

-record(area_object, {object, pos_x, pos_y}).
-record(area_state, {objects=[]}).
%%%
%%% 
%%%
-export([new/0, add/3, move/3, remove/2, list/1, position/2]).


%%% ------------------------------------------------
%%% Returns a new area reference.
%%% ------------------------------------------------
new() ->
	area_state#{}.
	
%%% ------------------------------------------------
%%% add(Area, Term, {PosX, PosY}) -> {ok, NewArea}
%%% Adds an object to the given area at the given
%%% position.
%%% ------------------------------------------------	
add(Area, Term, {PosX, PosY}) ->
	% Create a new object for the term
	Object = area_object#{object=Term, pos_x=PosX, pos_y=PosY},
	% And add it to the internal list
	NewArea = Area#area_state{objects=[Object | Area#area-state{objects}],
	{ok, NewArea}.
	
%%% ------------------------------------------------
%%% 
%%% ------------------------------------------------
move(Area, Term, {PosX, PosY}) ->
	ok.
	
%%% ------------------------------------------------
%%% 
%%% ------------------------------------------------	
remove(Area, Term) ->
	% Create the fun to return true for the given term
	Pred = fun(Ele) -> Ele#area_object{object} = Term end,
	
	% Drop the given element from the object list in the state
	ObjList = lists:dropwhile(Pred, Area#area_state{objects}),
	
	% Update the area object and return it
	{ok, Area#area_state{objects=ObjList}}.

%%% ------------------------------------------------
%%% 
%%% ------------------------------------------------
position(Area, Term) ->
	ok.

%%% ------------------------------------------------
%%% Returns a copy of the internal list of area_objects.
%%% ------------------------------------------------
list(Area) ->
	{ok, Area#area_state{objects}}.