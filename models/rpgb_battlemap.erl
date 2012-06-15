-module(rpgb_battlemap, [Id, Name :: binary(), OwnerId :: string(),
	Url :: binary(),
	% display settings
	Zoom :: float(), TranslateX :: integer(), TranslateY :: integer(),
	GridSpacing :: integer(), BackgroundColor :: binary(),
	GridlineColor :: binary(), GridOpacity :: float(),
	CreatedTime :: timestamp(), UpdatedTime :: timestamp()]).
-has({zones, many, [{module, rpgb_zone}]}).
-has({combatants, many, [{module, rpgb_combatant}]}).
-belongs_to(owner).
-has({participants, many, [{module, rpgb_participants}]}).
-compile([export_all]).

%-define(json_aliases, [
%	{<<"zoom">>, zoom}, {<<"translateX">>, translate_x},
%	{<<"translateY">>, translate_y}, {<<"gridSpacing">>, grid_spacing},
%	{<<"backgroundColor">>, background_color},
%	{<<"gridlineColor">>, gridline_color}, {<<"gridOpacity">>, grid_opacity}
%]).


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

json_enc_exclude() ->
	[owner].

json_dec_exlucde() ->
	[created_time, update_time, url, id, owner].

json_prop_names() ->
	[{Val, Key} || {Key, Val} <- [{<<"zoom">>, zoom},
	{<<"translateX">>, translate_x},
	{<<"translateY">>, translate_y}, {<<"gridSpacing">>, grid_spacing},
	{<<"backgroundColor">>, background_color},
	{<<"gridlineColor">>, gridline_color}, {<<"gridOpacity">>, grid_opacity}]].

%to_json() ->
%	Zones = THIS:zones(),
%	Zones0 = [Z:to_json() || Z <- Zones],
%	Combatants = THIS:combatants(),
%	Combatants0 = [C:to_json() || C <- Combatants],
%	FlippedAliases = [{Val, Key} || {Key, Val} <- ?json_aliases],
%	Id0 = list_to_binary(Id),
%	Created = rpgb_util:now_to_timestamp(CreatedTime),
%	Updated = rpgb_util:now_to_timestamp(UpdatedTime),
%	Attrs = THIS:attributes(),
%	UpdateAttr = [
%		{id, Id0},
%		{created_time, Created},
%		{updated_time, Updated}
%	],
%	Attrs0 = rpgb:set_proplist(UpdateAttr, Attrs),
%	Attrs1 = proplists:substitute_alieas([{created_time, <<"createdTime">>}, {updated_time, <<"updatedTime">>} | FlippedAliases], Attrs0),
%	{struct, Attrs1}.
%
%from_json(Binary) when is_binary(Binary) ->
%	{struct, _P} = Json = mochijson2:decode(Binary),
%	from_json(Json);
%
%from_json({struct, Props}) ->
%	Zones = proplists:get_value(<<"zones">>, Props, []),
%	Zones0 = [begin
%		Zone = boss_record:new(rpbg_zone, []),
%		Zone:from_json(Z)
%	end || Z <- Zones],
%
%	Combatants = proplists:get_value(<<"combatants">>, Props, []),
%	Combatants0 = [begin
%		Combatant = boss_record:new(rpgb_combatant, []),
%		Combatant:from_json(C)
%	end || C <- Combatants],
%
%	sanitize_json(Props).
%
%sanitize_json(JsonProps) ->
%	Clean0 = proplists:substitute_aliases(?json_aliases, JsonProps),
%	Attr = THIS:attribute_names(),
%	Clean1 = [Prop || {Key, _Val} = Prop <- Clean0, lists:member(Key, Attr)],
%	THIS:set(Clean1).
