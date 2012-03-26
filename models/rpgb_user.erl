-module(rpgb_user, [Id, Name::binary(), OpenID::binary(), RpgbGroupId, CreatedTime::timestamp(), UpdatedTime::timestamp()]).
-has({permissions, many, [{module, rpgb_permission}]}).
-belongs_to(rpgb_group).
-compile([export_all]).

before_create() ->
	[Name0, OpenID0] = [if
		is_list(X) -> list_to_binary(X);
		true -> X
	end || X <- [Name, OpenID]],
	io:format("bing"),
	{ok, ?MODULE:new(Id, Name0, OpenID0, RpgbGroupId, erlang:now(), erlang:now())}.

before_update() ->
	[Name0, OpenID0] = [if
		is_list(X) -> list_to_binary(X);
		true -> X
	end || X <- [Name, OpenID]],
	io:format("bang"),
	{ok, ?MODULE:new(Id, Name0, OpenID0, RpgbGroupId, CreatedTime, erlang:now())}.
