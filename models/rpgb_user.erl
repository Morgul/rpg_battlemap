-module(rpgb_user, [Id, Name::binary(), OpenID::binary(), RpgbGroupId, CreatedTime::timestamp(), UpdatedTime::timestamp()]).
-has({permissions, many, [{module, rpgb_permission}]}).
-belongs_to(rpgb_group).
-compile([export_all]).

before_create() ->
	{ok, ?MODULE:new(Id, Name, OpenID, RpgbGroupId, erlang:now(), erlang:now())}.

before_update() ->
	{ok, ?MODULE:new(Id, Name, OpenID, RpgbGroupId, CreatedTime, erlang:now())}.
