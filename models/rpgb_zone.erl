-module(rpgb_zone, [Id, Name :: binary(), BattlemapId :: atom(),
	Layer :: binary(), Url :: binary(), ZIndex :: integer(),
	Rotation :: binary(),
	StrokeOpactiy :: float(), StrokeColor :: binary(),
	Color :: binary(), Gappy :: boolean(), Path :: binary(),
	CreatedTime :: timestamp(), UpdatedTime :: timestamp()]).
-belongs_to(battlemap).
-compile([export_all]).

before_create() ->
	Now = erlang:now(),
	{ok, THIS:set([{created_time, Now}, {updated_time, Now}])}.

before_update() ->
	{ok, THIS:set([{updated_time, erlang:now()}])}.

json_enc_exclude() ->
	[battle].

json_dec_exclude() ->
	[created_time, updated_time, id].

json_prop_names() ->
	[{battle_id, <<"battleId">>}, {z_index, <<"zIndex">>},
	{stroke_opacity, <<"strokeOpacity">>}, {stroke_color, <<"strokeColor">>},
	{created_time, <<"createdTime">>}, {updated_time, <<"updated_time">>}].
