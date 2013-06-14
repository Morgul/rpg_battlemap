-module(rpgb_layers_tests).

-include("prop_tests.hrl").

%% === commands ==========

%% === precondtion =======

%% === next_state ========

%% === postconditions ====

%% === whatever ==========

layers_next_state(State, _Result, [_Transport, _Who, put, undefined, _Json, undefined]) ->
	State;

layers_next_state(State, _Result, [_Transport, _Who, get | _]) ->
	State;

layers_next_state(State, Result, [_Transport, Who, put, undefined, Json, Map]) ->
	case layer_put_valid(Json, undefined) of
		true ->
			BaseLayer = extract_layer(undefined, Who, Result),
			Layer = BaseLayer#test_layer{map_id = Map#test_map.id},
			Map2 = Map#test_map{layers = Map#test_map.layers ++ [Layer#test_layer.id]},
			Maps = lists:keyreplace(Map#test_map.id, #test_map.id, State#state.maps, Map2),
			Layers = State#state.layers ++ [Layer],
			State#state{maps = Maps, layers = Layers};
		false ->
			State
	end;

layers_next_state(State, Result, [_Transport, Who, put, Original, Json, _Map]) ->
	case layer_put_valid(Json, Original) of
		true ->
			Layer = extract_layer(Original, Who, Result),
			Layers = lists:keyreplace(Layer#test_layer.id, #test_layer.id, State#state.layers, Layer),
			State#state{layers = Layers};
		false ->
			State
	end;

layers_next_state(State, _Result, [_Transport, _Who, delete, undefined, _Json, _Map]) ->
	State;

layers_next_state(State, _Result, [_Transport, _Who, delete, _Original, _Json, #test_map{layers = Layers}]) when length(Layers) == 1 ->
	State;

layers_next_state(State, _Result, [_Transport, _Who, delete, undefined | _]) ->
	State;

layers_next_state(State, _Result, [_Transport, _Who, delete, Original, _Json, _AMap]) ->
	LayerId = Original#test_layer.id,
	Map = lists:keyfind(Original#test_layer.map_id, #test_map.id, State#state.maps),
	case Map of
		false ->
			?debugFmt("For whatever reason the map could not be found.~n"
				"    State: ~p~n"
				"    Layer: ~p", [State, Original]);
		_ ->
			ok
	end,
	MapLayers = lists:delete(LayerId, Map#test_map.layers),
	Map2 = Map#test_map{layers = MapLayers},
	Maps = lists:keyreplace(Map2#test_map.id, #test_map.id, State#state.maps, Map2),
	FilterFun = fun(ZoA) ->
		ZoA#test_zone.layer_id =/= LayerId
	end,
	Zones = lists:filter(FilterFun, State#state.zones),
	Layers = lists:keydelete(LayerId, #test_layer.id, State#state.layers),
	State#state{maps = Maps, layers = Layers, zones = Zones}.

layer_put_valid(Json, undefined) ->
	Name = proplists:get_value(<<"name">>, rpgb_requests_tests:fix_json_keys(Json)),
	if
		Name == undefined ->
			false;
		Name == <<>> ->
			false;
		true ->
			true
	end;
layer_put_valid(_,_) ->
	true.

extract_layer(Original, Who, {map, Result}) ->
	KeyBase = [<<"layers">>, 1],
	extract_layer(Original, Who, Result, KeyBase);

extract_layer(Original, Who, Result) ->
	KeyBase = [],
	extract_layer(Original, Who, Result, KeyBase).

extract_layer(Original, _Who, Result, KeyBase) ->
	IdKey = KeyBase ++ [<<"id">>],
	UrlKey = KeyBase ++ [<<"url">>],
	L1 = #test_layer{
		id = {call, rpgb_requests_tests, extract_json, [Result, IdKey]},
		url = {call, rpgb_requests_tests, extract_json, [Result, UrlKey]},
		properties = {call, rpgb_requests_tests, extract_json, [Result, KeyBase]}
	},
	case Original of
		undefined ->
			L1;
		_ ->
			L1#test_layer{
				id = Original#test_layer.id,
				map_id = Original#test_layer.map_id,
				url = Original#test_layer.url,
				zones = Original#test_layer.zones,
				auras = Original#test_layer.auras
			}
	end.
