-module(rpgb_combatant_template, [Id, OwnerId :: string(),
	Name :: string(), Color :: string(), Image :: string(), Url :: binary(),
	Size :: integer(), CreatedTime :: timestamp(),
	UpdatedTime :: timestamp()]).

-belongs_to(owner).

before_create() ->
	This0 = THIS:set([
		{created_time, erlang:now()}, {updated_time, erlang:now()}
	]),
	{ok, This0}.

before_update() ->
	This0 = THIS:set([{updated_time, erlang:now()}]),
	{ok, This0}.

to_json() ->
	OwnerURL = case boss_db:find(OwnerId) of
		{error, _} ->
			undefined;
		Rec ->
			Rec:url()
	end,
	Id0 = list_to_binary(Id),
	Created = rpgb:now_to_timestamp(CreatedTime),
	Updated = rpgb:now_to_timestamp(UpdatedTime),
	UpdateAttr = [
		{id, Id0},
		{created_time, Created},
		{updated_time, Updated},
		{owner, OwnerURL}
	],
	Attr = THIS:attributes(),
	Props = rpgb:set_proplist(UpdateAttr, Attr),
	{struct, Props}.
