-module(rpgb_rec_battlemap).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([make_json/1, make_json/3]).
-export([delete/1]).
-export([get_by_participant/1, is_user_participant/2]).
-export([update_from_json/2]).

make_json(Map) ->
	Layers = rpgb_rec_layer:get_map_layers(Map#rpgb_rec_battlemap.bottom_layer_id),
	Combatants = rpgb_rec_combatant:get_map_combatants(Map#rpgb_rec_battlemap.first_combatant_id),
	make_json(Map, Layers, Combatants).

make_json(Map, Layers, Combatants) ->
	Url = rpgb:get_url(["maps", integer_to_list(Map#rpgb_rec_battlemap.id)]),
	<<"http", RestUrl/binary>> = Url,
	WebSocket = <<"ws", RestUrl/binary, "/ws">>,
	MakeLayerJson = fun(InJson, _InMap) ->
		LayersJsons = [rpgb_rec_layer:make_json(Layer) || Layer <- Layers],
		[{layers, LayersJsons} | InJson]
	end,
	MakeCombatantJson = fun(InJson, _InMap) ->
		CombatantsJsons = [rpgb_rec_combatant:make_json(Combatant) || Combatant <- Combatants],
		[{combatants, CombatantsJsons} | InJson]
	end,
	Map:to_json([{url, Url},{websocketUrl, WebSocket}, bottom_layer_id, MakeLayerJson, first_combatant_id, MakeCombatantJson, fun expand_owner/2, fun expand_particpants/2]).

get_by_participant(#rpgb_rec_user{id = UserId}) ->
	get_by_participant(UserId);

get_by_participant(UserId) ->
	% This is a prime candidate for optimization
	{ok, Maps} = rpgb_data:search(rpgb_rec_battlemap, []),
	Filter = fun(#rpgb_rec_battlemap{owner_id = OwnerId, participant_ids = Partiers}) ->
			OwnerId == UserId orelse lists:member(UserId, Partiers)
	end,
	Maps2 = lists:filter(Filter, Maps),
	{ok, Maps2}.

delete(#rpgb_rec_battlemap{id = Id}) ->
	delete(Id);

delete(Id) ->
	Out = rpgb_data:delete(rpgb_rec_battlemap, Id),
	{ok, Layers} = rpgb_data:search(rpgb_rec_layer, [{battlemap_id, Id}]),
	[rpgb_rec_layer:delete(Layer) || Layer <- Layers],
	{ok, Combatants} = rpgb_data:search(rpgb_rec_combatant, [{battlemap_id, Id}]),
	[rpgb_rec_combatant:delete(Combatant) || Combatant <- Combatants],
	Out.

is_user_participant(#rpgb_rec_user{id = UserId}, #rpgb_rec_battlemap{owner_id = UserId}) ->
	true;
is_user_participant(User, Map) ->
	ParticipantIds = Map#rpgb_rec_battlemap.participant_ids,
	UserId = User#rpgb_rec_user.id,
	lists:member(UserId, ParticipantIds).

update_from_json(Json, InitMap) ->
	ValidateFuns = [
		fun scrub_disallowed/1,
		fun check_blank_name/1,
		fun check_name_conflict/1,
		fun validate_json/1,
		fun check_named_map/1,
		fun check_participants/1
	],
	case rpgb:bind({Json, InitMap}, ValidateFuns) of
		{ok, {_Json, Rec}} ->
			{ok, Rec};
		Else ->
			Else
	end.

check_participants({Json, Map}) ->
	ParticipantIds = proplists:get_value(<<"participant_ids">>, Json),
	if
		is_list(ParticipantIds) ->
			case check_participants(ParticipantIds, Map) of
				{error, _, _} = Out -> Out;
				Map2 ->
					{ok, {Json, Map2}}
			end;
		ParticipantIds =:= undefined ->
			{ok, {Json, Map}};
		true ->
			{error, {invalid, <<"invalid participant list">>}}
	end.

check_participants(Ids, Map) ->
	Validator = fun(Id) ->
		case rpgb_data:get_by_id(rpgb_rec_user, Id) of
			{error, notfound} ->
				false;
			_ ->
				true
		end
	end,
	case lists:all(Validator, Ids) of
		true ->
			Map#rpgb_rec_battlemap{participant_ids = Ids};
		false ->
			{error, {invalid, <<"invalid participant list">>}}
	end.

check_named_map({Json, Map}) ->
	MapName = Map#rpgb_rec_battlemap.name,
	JsonName = proplists:get_value(<<"name">>, Json),
	case {MapName, JsonName} of
		{undefined, undefined} ->
			{error, {invalid, <<"Name cannot be blank.">>}};
		_ ->
			{ok, {Json, Map}}
	end.

check_blank_name({Json, Map}) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, {invalid, <<"Name cannot be blank.">>}};
		null ->
			{error, {invalid, <<"Name cannot be blank.">>}};
		Name when is_binary(Name) ->
			{ok, {Json, Map}};
		undefined when is_binary(Map#rpgb_rec_battlemap.name) ->
			{ok, {Json, Map}};
		undefined ->
			{error, {invalid, <<"Name cannot be blank.">>}};
		_ ->
			{error, {invalid, <<"Name must be a word.">>}}
	end.

check_name_conflict({Json, Map}) ->
	#rpgb_rec_battlemap{owner_id = Owner, name = MapName} = Map,
	case proplists:get_value(<<"name">>, Json) of
		undefined ->
			{ok, {Json, Map}};
		MapName ->
			{ok, {Json, Map}};
		OtherName ->
			Searched = rpgb_data:search(rpgb_rec_battlemap, [
				{name, OtherName}, {owner_id, Owner}]),
			case Searched of
				{ok, []} ->
					{ok, {Json, Map}};
				_ ->
					{error, {conflict, <<"You already have a map by that name.">>}}
			end
	end.

scrub_disallowed({Json, Map}) ->
	{ok, Json2} = scrub_disallowed(Json),
	{ok, {Json2, Map}};

scrub_disallowed([{}]) ->
	{ok, [{}]};

scrub_disallowed(Json) ->
	Disallowed = [<<"id">>, <<"owner_id">>, <<"created">>, <<"updated">>,
		<<"zoom">>, <<"translate_x">>, <<"translate_y">>, <<"grid_spacing">>,
		<<"bottom_layer_id">>, <<"first_combatant_id">>],
	Disallowed1 = ordsets:from_list(Disallowed),
	Json1 = ordsets:from_list(Json),
	scrub_disallowed(Json1, Disallowed1).

scrub_disallowed(Json, []) ->
	{ok, Json};

scrub_disallowed(Json, [Nope | Tail] = Nopes) ->
	case proplists:delete(Nope, Json) of
		Json ->
			scrub_disallowed(Json, Tail);
		Json1 ->
			scrub_disallowed(Json1, Nopes)
	end.

validate_json({Json, Map}) ->
	case validate_json(Json, Map) of
		{ok, Map2} ->
			{ok, {Json, Map2}};
		{error, {bad_color, Key}} ->
			Body = iolist_to_binary(io_lib:format("invalid color for ~s.", [Key])),
			{error, {invalid, Body}}
	end;

validate_json(Json) ->
	case rpgb_rec_battlemap:from_json(Json) of
		{ok, Rec, Warnings} ->
			validate_warnings(Warnings, Rec);
		Else ->
			Else
	end.

validate_json(Json, Rec) ->
	{ok, Json1} = scrub_disallowed(Json),
	case rpgb_rec_battlemap:from_json(Rec, Json1) of
		{ok, Rec1, Warnings} ->
			validate_warnings(Warnings, Rec1);
		Else ->
			Else
	end.

validate_warnings([], Rec) ->
	{ok, Rec};

validate_warnings([background_color | Tail], Rec) ->
	Color = Rec#rpgb_rec_battlemap.background_color,
	case validate_color(Color) of
		true ->
			validate_warnings(Tail, Rec);
		false ->
			{error, {bad_color, background_color}}
	end;

validate_warnings([gridline_color | Tail], Rec) ->
	Color = Rec#rpgb_rec_battlemap.gridline_color,
	case validate_color(Color) of
		true ->
			validate_warnings(Tail, Rec);
		false ->
			{error, {bad_color, gridline_color}}
	end.

validate_color(Color) ->
	rpgb_validation:is_valid_color(Color).

expand_owner(Json, Map) ->
	OwnerName = case rpgb_data:get_by_id(rpgb_rec_user, Map#rpgb_rec_battlemap.owner_id) of
		{error, notfound} ->
			null;
		{ok, User} ->
			User#rpgb_rec_user.name
	end,
	[{<<"owner">>, OwnerName} | Json].

expand_particpants(Json, Map) ->
	PartyNames = lists:foldl(fun(Id, Acc) ->
		case rpgb_data:get_by_id(rpgb_rec_user, Id) of
			{error, _} ->
				Acc;
			{ok, User} ->
				[User#rpgb_rec_user.name | Acc]
		end
	end, [], Map#rpgb_rec_battlemap.participant_ids),
	[{<<"participant">>, lists:reverse(PartyNames)} | Json].
