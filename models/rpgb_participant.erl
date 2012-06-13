-module(rpgb_participant, [Id, BattleId :: string(), UserId :: string(),
	Url :: binary()]).
-belongs_to(user).
-belongs_to(battle).

to_json() ->
	Attr = THIS:attributes(),
	BattleUrl = case boss_db:find(BattleId) of
		{error, _} -> undefined;
		Brec -> Brec:url()
	end,
	User = case boss_db:find(UserId) of
		{error, _} -> undefined;
		Urec -> Urec:to_json()
	end,
	Id0 = list_to_binary(Id),
	UpdateAttr = [
		{id, Id0},
		{battlemap, BattleUrl},
		{user, User}
	],
	Attr0 = rpgb:set_proplist(UpdateAttr, Attr),
	{struct, Attr0}.
