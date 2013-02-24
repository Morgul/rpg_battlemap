-module(rpgb_zone).

-include("rpg_battlemap.hrl").

-export([make_json/4, make_json/5]).
-export([get_layer_zones/1]).
-export([delete/1]).

make_json(Req, Host, Port, Zone) ->
	{ok, Layer} = rpgb_data:get_by_id(rpgb_rec_layer, Zone#rpgb_rec_zone.layer_id),
	make_json(Req, Host, Port, Zone, Layer#rpgb_rec_layer.battlemap_id).

make_json(Req, Host, Port, Zone, MapId) ->
	Path = iolist_to_binary(io_lib:format("map/~p/layers/~p/~ss/~p", [
		MapId, Zone#rpgb_rec_zone.layer_id, Zone#rpgb_rec_zone.type, Zone#rpgb_rec_zone.id])),
	Url = rpgb:get_url(Req, Host, Port, Path),
	Zone:to_json([{url, Url}, type, {null_is_undefined}, fun fix_attrs/2]).

fix_attrs(Json, #rpgb_rec_zone{element_attrs = []}) ->
	lists:keystore(<<"element_attrs">>, 1, Json, {<<"element_attrs">>, [{}]});
fix_attrs(Json, _Rec) ->
	Json.

get_layer_zones(IntialId) ->
	Got = rpgb_data:get_by_id(rpgb_rec_zone, IntialId),
	get_layer_zones(Got, []).

get_layer_zones({error, notfound}, Acc) ->
	lists:reverse(Acc);

get_layer_zones({ok, Zone}, Acc) ->
	Got = rpgb_data:get_by_id(rpgb_rec_zone, Zone#rpgb_rec_zone.next_zone_id),
	get_layer_zones(Got, [Zone | Acc]).

delete(#rpgb_rec_zone{id = Id}) ->
	delete(Id);

delete(Id) ->
	rpgb_data:delete(rpgb_rec_zone, Id).