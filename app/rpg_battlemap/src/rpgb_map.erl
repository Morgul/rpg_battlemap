-module(rpgb_map).

-include("rpg_battlemap.hrl").

-export([make_json/4, make_json/6]).
-export([delete/1]).

make_json(Proto, Host, Port, Map) ->
	Layers = rpgb_layer:get_map_layers(Map#rpgb_rec_battlemap.bottom_layer_id),
	Combatants = rpgb_combatant:get_map_combatants(Map#rpgb_rec_battlemap.first_combatant_id),
	make_json(Proto, Host, Port, Map, Layers, Combatants).

make_json(Proto, Host, Port, Map, Layers, Combatants) ->
	Url = rpgb:get_url(Proto, Host, Port, ["map", integer_to_list(Map#rpgb_rec_battlemap.id)]),
	<<"http", RestUrl/binary>> = Url,
	WebSocket = <<"ws", RestUrl/binary>>,
	MakeLayerJson = fun(InJson, _InMap) ->
		LayersJsons = [rpgb_layer:make_json(Proto, Host, Port, Layer) || Layer <- Layers],
		[{layers, LayersJsons} | InJson]
	end,
	MakeCombatantJson = fun(InJson, _InMap) ->
		CombatantsJsons = [rpgb_combatant:make_json(Proto, Host, Port, Combatant) || Combatant <- Combatants],
		[{combatants, CombatantsJsons} | InJson]
	end,
	Map:to_json([{url, Url},{websocketUrl, WebSocket}, bottom_layer_id, MakeLayerJson, first_combatant_id, MakeCombatantJson]).

delete(#rpgb_rec_battlemap{id = Id}) ->
	delete(Id);

delete(Id) ->
	Out = rpgb_data:delete(rpgb_rec_battlemap, Id),
	{ok, Layers} = rpgb_data:search(rpgb_rec_layer, [{battlemap_id, Id}]),
	[rpgb_layer:delete(Layer) || Layer <- Layers],
	{ok, Combatants} = rpgb_data:search(rpgb_rec_combatant, [{battlemap_id, Id}]),
	[rpgb_combatant:delete(Combatant) || Combatant <- Combatants],
	Out.
