-module(rpgb_rec_battlemap).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([make_json/4, make_json/6]).
-export([delete/1]).
-export([get_by_participant/1]).

make_json(Req, Host, Port, Map) ->
	Layers = rpgb_layer:get_map_layers(Map#rpgb_rec_battlemap.bottom_layer_id),
	Combatants = rpgb_combatant:get_map_combatants(Map#rpgb_rec_battlemap.first_combatant_id),
	make_json(Req, Host, Port, Map, Layers, Combatants).

make_json(Req, Host, Port, Map, Layers, Combatants) ->
	Url = rpgb:get_url(Req, Host, Port, ["map", integer_to_list(Map#rpgb_rec_battlemap.id)]),
	<<"http", RestUrl/binary>> = Url,
	WebSocket = <<"ws", RestUrl/binary>>,
	MakeLayerJson = fun(InJson, _InMap) ->
		LayersJsons = [rpgb_layer:make_json(Req, Host, Port, Layer) || Layer <- Layers],
		[{layers, LayersJsons} | InJson]
	end,
	MakeCombatantJson = fun(InJson, _InMap) ->
		CombatantsJsons = [rpgb_combatant:make_json(Req, Host, Port, Combatant) || Combatant <- Combatants],
		[{combatants, CombatantsJsons} | InJson]
	end,
	Map:to_json([{url, Url},{websocketUrl, WebSocket}, bottom_layer_id, MakeLayerJson, first_combatant_id, MakeCombatantJson]).

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
	[rpgb_layer:delete(Layer) || Layer <- Layers],
	{ok, Combatants} = rpgb_data:search(rpgb_rec_combatant, [{battlemap_id, Id}]),
	[rpgb_combatant:delete(Combatant) || Combatant <- Combatants],
	Out.
