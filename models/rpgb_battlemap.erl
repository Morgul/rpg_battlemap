-module(rpgb_battlemap, [Id, Name :: binary(), OwnerId :: string(),
	Settings :: binary(), Url :: binary(), CreatedTime :: timestamp(),
	UpdatedTime :: timestamp()]).
-has({rpgb_zone, many}).
-has({rpgb_combatant, many}).
-belongs_to(owner).
-has({rpgb_participants, many}).
-compile([export_all]).

before_create() ->
	This0 = THIS:set([
		{created_time, erlang:now()},
		{updated_time, erlang:now()}
	]),
	{ok, This0}.

before_update() ->
	This0 = THIS:set([
		{updated_time, erlang:now()}
	]),
	{ok, This0}.

to_json() ->
	Zones = THIS:rpgb_zones(),
	Zones0 = [Z:to_json() || Z <- Zones],
	Combatants = THIS:rpgb_combatants(),
	Combatants0 = [C:to_json() || C <- Combatants],
	{struct, Props} = THIS:settings(),
	Id0 = list_to_binary(Id),
	Created = rpgb_util:now_to_timestamp(CreatedTime),
	Updated = rpgb_util:now_to_timestamp(UpdatedTime),
	Attrs = THIS:attributes(),
	UpdateAttr = [
		{id, Id0},
		{created_time, Created},
		{updated_time, Updated}
	],
	Attrs0 = rpgb:set_proplist(UpdateAttr, Attrs),
	{struct, lists:append(Props, Attrs0)}.
