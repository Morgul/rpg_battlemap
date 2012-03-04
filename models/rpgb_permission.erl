-module(rpgb_permission, [Id, Name::string(), ForeignId::string()]).
-compile(export_all).

before_create() ->
	Id0 = Name ++ "-" ++ ForeignId,
	{ok, ?MODULE:new(Id0, Name, ForeignId)}.

before_update() ->
	Id0 = Name ++ "-" ++ ForeignId,
	{ok, ?MODULE:new(Id0, Name, ForeignId)}.
