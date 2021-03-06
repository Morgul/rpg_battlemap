-module(rpgb_rec_layer).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([make_json/1, make_json/3]).
-export([get_map_layers/1]).
-export([delete/1]).

make_json(Layer) ->
	Zones = rpgb_rec_zone:get_layer_zones(Layer#rpgb_rec_layer.first_zone_id),
	Auras = rpgb_rec_zone:get_layer_zones(Layer#rpgb_rec_layer.first_aura_id),
	make_json(Layer, Zones, Auras).

make_json(Layer, Zones, Auras) ->
	Url = rpgb:get_url(["maps", integer_to_list(Layer#rpgb_rec_layer.battlemap_id), "layers", integer_to_list(Layer#rpgb_rec_layer.id)]),
	ZoneJsons = [rpgb_rec_zone:make_json(Zone, Layer#rpgb_rec_layer.battlemap_id) || Zone <- Zones],
	AurasJsons = [rpgb_rec_zone:make_json(Aura, Layer#rpgb_rec_layer.battlemap_id) || Aura <- Auras],
	Layer:to_json([{url, Url}, {zones, ZoneJsons}, {auras, AurasJsons}, first_aura_id, first_zone_id]).

get_map_layers(InitialId) ->
	GotLayer = rpgb_data:get_by_id(rpgb_rec_layer, InitialId),
	get_map_layers(GotLayer, []).

get_map_layers({error, notfound}, Acc) ->
	lists:reverse(Acc);
get_map_layers({ok, Layer}, Acc) ->
	NextLayer = rpgb_data:get_by_id(rpgb_rec_layer, Layer#rpgb_rec_layer.next_layer_id),
	get_map_layers(NextLayer, [Layer | Acc]).

delete(#rpgb_rec_layer{id = Id}) ->
	delete(Id);

delete(Id) ->
	Out = rpgb_data:delete(rpgb_rec_layer, Id),
	{ok, Zones} = rpgb_data:search(rpgb_rec_zone, [{layer_id, Id}]),
	[rpgb_rec_zone:delete(Zone) || Zone <- Zones],
	Out.