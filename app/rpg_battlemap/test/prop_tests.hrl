-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-undef(debugMsg).
-undef(debugFmt).
-define(debugMsg(_Msg), ok).
-define(debugFmt(_Fmt,_Args),ok).

-define(accepts, {"Accepts", "application/json"}).
-define(contenttype, {"Content-Type", "application/json"}).
-define(headers(Headers), [?accepts, ?contenttype | Headers]).
-define(cookie(Type), {"Cookie", rpgb_requests_tests:cookie(Type)}).

-compile(export_all).

-record(test_map, {
	id, url, ws_url, owner, name, properties, combatants = [], layers = []
}).
-record(test_character, {
	id, url, owner, name, properties
}).
-record(test_combatant, {
	id, url, map_id, owner, properties
}).
-record(test_layer, {
	id, url, map_id, properties, name, zones = [], auras = []
}).
-record(test_zone, {
	id, url, map_id, layer_id, type, name, properties
}).
-record(state, {
	maps = [] :: [#test_map{}],
	combatants = [],
	characters = [],
	layers = [],
	zones = [],
	ws = []
}).
-record(ws_msg, {
	id, action, data, accepted, type_id, type
}).
