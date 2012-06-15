-module(rpgb_combatant, [Id, BattlemapId :: atom(), CombatantTemplateId :: string(), 
	X :: integer(), Y :: integer(), Initiative :: float(), Size :: integer(),
	Url :: binary(), CreatedTime :: timestamp(), UpdatedTime :: timestamp()]).
-belongs_to(combatant_template).
-belongs_to(battlemap).

-compile([export_all]).

before_create() ->
	This0 = THIS:set([
		{created_time, erlang:now()}, {updated_time, erlang:now()}
	]),
	{ok, This0}.

before_update() ->
	This0 = THIS:set([
		{updated_time, erlang:now()}
	]),
	{ok, This0}.

%to_json() ->
%	BattleURL = case boss_db:find(BattleId) of
%		{error, _} -> undefined;
%		BattleRec -> BattleRec:url()
%	end,
%	Id0 = list_to_binary(Id),
%	Created = rpgb:now_to_timestamp(CreatedTime),
%	Updated = rpgb:now_to_timestamp(UpdatedTime),
%	Templated = case boss_db:find(CombatantId) of
%		{error, _} -> undefined;
%		CombatantTemplate ->
%			CombatantTemplate:to_json()
%	end,
%	UpdateAttr = [
%		{id, Id0},
%		{created_time, Created},
%		{updated_time, Updated},
%		{combatant_template, Templated},
%		{battlemap, BattleURL}
%	],
%	Attr = THIS:attributes(),
%	Attr0 = rpgb:set_proplist(UpdateAttr, Attr),
%	{struct, Attr0}.
