-module(rpgb_user, [Id, Name::binary(), OpenID::binary(),
	RpgbGroupId::string(), MaxMaps :: integer(), CreatedTime::timestamp(),
	UpdatedTime::timestamp()]).
-has({permissions, many, [{module, rpgb_permission}]}).
-has({battlemaps, many, [{module, rpgb_battlemap}]}).
-belongs_to(rpgb_group).
-compile([export_all]).

before_create() ->
	[Name0, OpenID0] = [if
		is_list(X) -> list_to_binary(X);
		true -> X
	end || X <- [Name, OpenID]],
	io:format("bing"),
	This0 = THIS:set([{name, Name0}, {open_id, OpenID0},
		{created_time, erlang:now()}, {updated_time, erlang:now()},
		{max_maps, case THIS:max_maps() of undefined -> -1; E -> E end}
	]),
	{ok, This0}.

before_update() ->
	[Name0, OpenID0] = [if
		is_list(X) -> list_to_binary(X);
		true -> X
	end || X <- [Name, OpenID]],
	io:format("bang"),
	This0 = THIS:set([{name, Name0}, {open_id, OpenID0},
		{updated_time, erlang:now()}
	]),
	{ok, This0}.
