-module(rpgb_battlemap, [Id, Name :: binary(), OwnerId :: string(),
	Json :: binary(), CreatedTime :: timestamp(),
	UpdatedTime :: timestamp()]).
-has({rpgb_zone, many}).
-has({rpgb_combatant, many}).
-belongs_to(owner).
-has({rpgb_participants, many}).
-compile([export_all]).

before_crete() ->
	{ok, ?MODULE:new(Id, Name, OwnerId, Json, erlang:now(), erlang:now())}.

before_update() ->
	{ok, ?MODULE:new(Id, Name, OwnerId, Json, CreatedTime, erlang:now())}.
