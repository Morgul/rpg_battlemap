-module(rpgb_group, [Id, Name::binary(), CreatedTime::timestamp(), UpdatedTime::timestamp()]).
-has({permissions, many}).
-compile([export_all]).

before_create() ->
	{ok, ?MODULE:new(Id, Name, erlang:now(), erlang:now())}.

before_update() ->
	{ok, ?MODULE:new(Id, Name, CreatedTime, erlang:now())}.
