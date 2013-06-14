-module(rpgb_requests_tests).

-include("prop_tests.hrl").

request_test_() -> {setup, fun() ->
		ok
	end,
	fun(_) ->
		application:stop(rpg_battlemap)
	end,
	fun(_) -> [

		{"statem", timeout, 60000, fun() ->
			?assertEqual(true, proper:quickcheck(?MODULE:prop_statem(), [{to_file, user}]))
		end}

	] end}.

prop_statem() ->
	?FORALL(Cmds, commands(?MODULE), begin
		{Hist, State, Res} = run_commands(?MODULE, Cmds),
		aggregate(command_names(Cmds), ?WHENFAIL(
			?debugFmt("~n"
				"==========================~n"
				"== proper check failed! ==~n"
				"==========================~n"
				"== Commands ==~n"
				"~p~n~n"
				"== Hstory ==~n"
				"~p~n~n"
				"== State ==~n"
				"~p~n~n"
				"== Result ==~n"
				"~p~n", [extended_commands(Cmds), Hist, State, Res]),
			Res == ok))
	end).

up() ->
	{setup, Up, _Down, _Tests} = request_test_(),
	Up().

extended_commands(Cmds) ->
	[extended_command(Cmd) || Cmd <- Cmds].

extended_command({set, _Var, {call, _Mod, connect_ws, [Who, Map]}}) ->
	{connect_ws, [Who, Map#test_map.id]};
extended_command({set, _Var, {call, _Mod, disconnect_ws, [{_Ws, Who, MapId}]}}) ->
	{disconnect_ws, [Who, MapId]};
extended_command({set, _Var, {call, Mod, Cmd, Args}}) ->
	Args2 = Mod:simple_args(Cmd, Args),
	{Mod, Cmd, Args2}.

simple_args([Transport, Who, Action, Orig | _]) ->
	[Transport, Who, Action, id(Orig)].

id(undefined) -> undefined;
id(#test_map{id = I}) -> I;
id(#test_character{id = I}) -> I;
id(#test_combatant{id = I}) -> I;
id(#test_zone{id = I}) -> I;
id(#test_layer{id = I}) -> I.

%command_category(Cmds) ->
%	Fun = fun
%		({set, _, {call, _Mod, connect_ws, [Who | _]}}) ->
%			{connect_ws, Who};
%		({set, _, {call, _Mod, disconnect_ws, _}}) ->
%			disconnect_ws;
%		({set, _, {call, _Mod, Resource, [Transport, Who, Action]}) ->
%			Trans = case Transport of
%				http -> http;
%				_ -> websocket
%			end,
%			{Resource, Trans, Who, Action}
%	end,
%	lists:map(Fun, Cmds).

%% ===================================================================
%% generators
%% ===================================================================

initial_state() ->
	AppEnvs = [
		{protocol, http},
		{hostname, "localhost"},
		{port, 10001},
		{listeners, 2},
		{data_callback, rpgb_dets},
		{additional_modules, [
			{rpgb_dets, rpgb_dets, start_link, [[{data_dir, "."}]], [rpgb_dets]}
		]}
	],
	[application:set_env(rpg_battlemap, Key, Val) || {Key, Val} <- AppEnvs],
	application:stop(rpg_battlemap),
	rpgb_test_util:start_app(rpg_battlemap),
	reset_data(),
	ibrowse:start(),
	{ok, _OwnerSession} = rpgb_test_util:create_authed_session(<<"owner_session">>, <<"owner">>),
	{ok, _Partier1Session} = rpgb_test_util:create_authed_session(<<"partier1_session">>, <<"partier1">>),
	{ok, _Partier2Session} = rpgb_test_util:create_authed_session(<<"partier2_session">>, <<"partier2">>),
	_NotParty = rpgb_test_util:create_authed_session(<<"notpartier_session">>, <<"notpartier">>),
	#state{}.

reset_data() ->
	Types = [rpgb_rec_user, rpgb_rec_battlemap, rpgb_rec_layer,
		rpgb_rec_character, rpgb_rec_combatant, rpgb_rec_zone],
	[delete_all(Type) || Type <- Types].

delete_all(Recname) ->
	{ok, Things} = rpgb_data:search(Recname, []),
	[rpgb_data:delete(Thing) || Thing <- Things].

command(#state{maps = []} = State) ->
	{call, rpgb_maps_tests, http, [g_who(), put, undefined, rpgb_prop:g_mapjson()]};
command(State) ->
	oneof([
		%{call, ?MODULE, characters, [g_transport(State), g_who(), g_action(), g_maybe_exists(State#state.characters), rpgb_prop:g_characterjson()]},
		%{call, ?MODULE, combatants, [g_transport(State), g_who(), g_action(), g_maybe_exists(State#state.combatants), rpgb_prop:g_combatantjson(), g_maybe_exists(State#state.maps)]},
		{call, rpgb_maps_tests, http, [g_who(), g_action(), g_maybe_exists(State#state.maps), rpgb_prop:g_mapjson()]},
		{call, rpgb_maps_tests, websocket, [g_maybe_exists(State#state.ws), g_action(), rpgb_prop:g_mapjson()]}]
		%{call, ?MODULE, layers, [g_transport(State), g_who(), g_action(), g_maybe_exists(State#state.layers), rpgb_prop:g_layerjson(), g_maybe_exists(State#state.maps)]},
		%{call, ?MODULE, zones, [g_transport(State), g_who(), g_action(), g_maybe_exists(State#state.zones), rpgb_prop:g_zonejson(), g_maybe_exists(State#state.layers)]},
		%{call, ?MODULE, auras, [g_transport(State), g_who(), g_action(), g_maybe_exists(State#state.zones), rpgb_prop:g_zonejson(), g_maybe_exists(State#state.layers)]},
		++ [{call, ?MODULE, connect_ws, [g_who(), elements(State#state.maps)]} || length(State#state.ws) =< 10]
		++ [{call, ?MODULE, disconnect_ws, [elements(State#state.ws)]} || State#state.ws =/= []]
	).

g_action() ->
	oneof([get, put, delete]).

g_who() ->
	frequency([
		{9, owner},
		{9, partier1},
		{9, partier2},
		{1, notpartier},
		{1, notauthed}
	]).

%g_maybe_exists([]) ->
%	undefined;
g_maybe_exists(List) ->
	oneof([undefined | List]).

g_transport(#state{ws = []}) ->
	http;
g_transport(State) ->
	oneof([http | State#state.ws]).

%% ===================================================================
%% preconditions
%% ===================================================================

precondition(State, {call, _Mod, connect_ws, [Who]}) ->
	not lists:keymember(Who, 1, State#state.ws);
precondition(State, {call, _Mod, connect_ws, [Who, Map]}) ->
	Sockets = [S || {_, W, M} = S <- State#state.ws, W =:= Who, M =:= Map#test_map.id],
	length(Sockets) == 0;
precondition(_State, {call, ?MODULE, _Call, _Args}) ->
	true;
precondition(State, {call, Mod, Call, Args}) ->
	Mod:precondition(State, {call, Mod, Call, Args}).
%precondition(_State, {call, _Mod, _Action, [websocket, nopartier | _Args]}) ->
%	false;
%precondition(State, {call, _Mod, _Action, [websocket, Who | _Args]}) ->
%	lists:keymember(Who, 1, State#state.ws);
%precondition(_State, {call, _mod, maps, [http, notpartier, put, undefined | _]}) ->
%	false;
%precondition(_State, {call, _Mod, combatants, [http, _Who, _Action, undefined, _Json, undefined | _]}) ->
%	false;
%precondition(_State, {call, _Mod, layers, [http, _Who, _Action, undefined, _HJson, undefined]}) ->
%	false;
%precondition(_State, {call, _Mod, ZoneOrAura, [_Transport, _Who, _Action, undefined, _Json, undefined]}) when ZoneOrAura =:= zones; ZoneOrAura =:= auras ->
%	false;
%precondition(_State, _Call) ->
%	true.

%% ===================================================================
%% next_state
%% ===================================================================

next_state(State, _Result, {call, _Mod, _Resource, [_Transport, notpartier | _]}) ->
	State;

%next_state(State, Result, {call, _Mod, characters, Args}) ->
%	characters_next_state(State, Result, Args);
%
%next_state(State, Result, {call, _Mod, maps, Args}) ->
%	maps_next_state(State, Result, Args);
%
%next_state(State, Result, {call, _Mod, combatants, Args}) ->
%	combatants_next_state(State, Result, Args);
%
%next_state(State, Result, {call, _Mod, layers, Args}) ->
%	layers_next_state(State, Result, Args);
%
%next_state(State, Result, {call, _Mod, zones, Args}) ->
%	zones_next_state(State, Result, Args);
%
%next_state(State, Result, {call, _Mod, auras, Args}) ->
%	auras_next_state(State, Result, Args);

next_state(State, _Result, {call, _Mod, connect_ws, [notauthed, _Map]}) ->
	State;

next_state(State, _Result, {call, _Mod, connect_ws, [notpartier, _Map]}) ->
	State;

next_state(State, Result, {call, _Mod, connect_ws, [Who, Map]}) ->
	Sockets = lists:keystore(Result, 1, State#state.ws, {Result, Who, Map#test_map.id}),
	State#state{ws = Sockets};

next_state(State, _Result, {call, _Mod, disconnect_ws, [{Socket, _Who, _MapId}]}) ->
	Sockets = lists:keydelete(Socket, 1, State#state.ws),
	State#state{ws = Sockets};

next_state(State, Result, {call, Mod, _Func, _Args} = Call) ->
	Mod:next_state(State, Result, Call);

next_state(State, Result, Call) ->
	?debugFmt("Fall through next state:~n"
		"    State: ~p~n"
		"    Result: ~p~n"
		"    Call: ~p", [State, Result, Call]),
	init:stop().

%% === characters ====================================================

characters_next_state(State, _Result, [_Transport, _Who, get, _Exists, _Json]) ->
	State;

characters_next_state(State, Result, [_Transport, Who, put, Orignal, Json]) ->
	case character_put_valid(Json, Orignal) of
		false ->
			State;
		true ->
			Character = extract_character(Orignal, Who, Result),
			Characters = lists:keystore(Character#test_character.id, #test_character.id, State#state.characters, Character),
			State#state{characters = Characters}
	end;

characters_next_state(State, _Result, [_Transport, _Who, delete, undefined, _Json]) ->
	State;

characters_next_state(State, _Result, [_Transport, _Who, delete, Char, _Json]) ->
	Id = Char#test_character.id,
	Characters = lists:keydelete(Id, #test_character.id, State#state.characters),
	State#state{characters = Characters}.

character_put_valid(Json, undefined) ->
	proplists:get_value(<<"name">>, Json) =/= undefined;
character_put_valid(_,_) ->
	true.

extract_character(undefined, Who, Result) ->
	#test_character{
	 id = {call, ?MODULE, extract_json, [Result, <<"id">>]},
	 url = {call, ?MODULE, extract_json, [Result, <<"url">>]},
	 properties = {call, ?MODULE, extract_json, [Result]},
	 owner = Who
	};

extract_character(Orig, _Who, Result) ->
	Orig#test_character{
		properties = {call, ?MODULE, extract_json, [Result]}
	}.

%% === combatants ====================================================

combatants_next_state(State, _Result, [_Transport, nopartier | _]) ->
	State;

combatants_next_state(State, _Result, [_Transport, _Who, get | _]) ->
	State;

combatants_next_state(State, _Result, [_Transport, _Who, put, undefined, undefined, _Json]) ->
	State;

combatants_next_state(State, Result, [_Transport, Who, put, undefined, Json, Map]) ->
	case combatant_put_valid(Json, undefined) of
		false ->
			State;
		true ->
			BaseCombatant = extract_combatant(undefined, Who, Result),
			Combatant = BaseCombatant#test_combatant{map_id = Map#test_map.id},
			Map2 = Map#test_map{combatants = Map#test_map.combatants ++ [Combatant#test_combatant.id]},
			Maps = lists:keyreplace(Map2#test_map.id, #test_map.id, State#state.maps, Map2),
			Combatants = State#state.combatants ++ [Combatant],
			State#state{maps = Maps, combatants = Combatants}
	end;

combatants_next_state(State, Result, [_Transport, Who, put, Original, Json, _Map]) ->
	case combatant_put_valid(Json, Original) of
		false ->
			State;
		true ->
			Combatant = extract_combatant(Original, Who, Result),
			Combatants = lists:keyreplace(Combatant#test_combatant.id, #test_combatant.id, State#state.combatants, Combatant),
			State#state{combatants = Combatants}
	end;

combatants_next_state(State, _Resut, [_Transport, _Who, delete, undefined, _Json, _Map]) ->
	State;

combatants_next_state(State, _Result, [_Transport, _Who, delete, Original, _Json, _Map]) ->
	Combatants = lists:keydelete(Original#test_combatant.id, #test_combatant.id, State#state.combatants),
	Map = lists:keyfind(Original#test_combatant.map_id, #test_map.id, State#state.maps),
	MapCombatants = lists:delete(Original#test_combatant.id, Map#test_map.combatants),
	Map2 = Map#test_map{combatants = MapCombatants},
	Maps = lists:keyreplace(Map2#test_map.id, #test_map.id, State#state.maps, Map2),
	State#state{maps = Maps, combatants = Combatants}.

combatant_put_valid(Json, undefined) ->
	Name = proplists:get_value(<<"name">>, fix_json_keys(Json)),
	if
		Name == undefined ->
			false;
		Name == <<>> ->
			false;
		true ->
			true
	end;
combatant_put_valid(_,_) ->
	true.

extract_combatant(undefined, Who, Result) ->
	#test_combatant{
		id = {call, ?MODULE, extract_json, [Result, <<"id">>]},
		url = {call, ?MODULE, extract_json, [Result, <<"url">>]},
		owner = Who,
		properties = {call, ?MODULE, extract_json, [Result]}
	};

extract_combatant(Original, _Who, Result) ->
	Original#test_combatant{
		properties = {call, ?MODULE, extract_json, [Result]}
	}.

%% === layers ========================================================


%% === zones =========================================================

zones_next_state(State, _Result, [_Transport, _Who, get | _]) ->
	State;

zones_next_state(State, _Result, [_Transport, _Who, put, undefined, _Json, undefined]) ->
	State;

zones_next_state(State, Result, [_Transport, _Who, put, undefined, Json, #test_layer{map_id = MapId} = Layer]) ->
	case zoneaura_put_valid(Json, undefined) of
		true ->
			BaseZone = extract_zoneaura(undefined, Result),
			Zone = BaseZone#test_zone{type = zone, map_id = MapId, layer_id = Layer#test_layer.id},
			Zones = State#state.zones ++ [Zone],
			Layer2 = Layer#test_layer{zones = Layer#test_layer.zones ++ [Zone#test_zone.id]},
			Layers = lists:keyreplace(Layer2#test_layer.id, #test_layer.id, State#state.layers, Layer2),
			State#state{layers = Layers, zones = Zones};
		false ->
			State
	end;

zones_next_state(State, _Result, [_Transport, _Who, put, undefined, _Json, _Layer]) ->
	State;

zones_next_state(State, Result, [_Transport, _Who, put, Original, Json, _Layer]) ->
	case zoneaura_put_valid(Json, Original) of
		true ->
			Zone = extract_zoneaura(Original, Result),
			Zones = lists:keyreplace(Zone#test_zone.id, #test_zone.id, State#state.zones, Zone),
			State#state{zones = Zones};
		false ->
			State
	end;

zones_next_state(State, _Result, [_Transport, _Who, delete, undefined, _Json, _Layer]) ->
	State;

zones_next_state(State, _Result, [_Transport, _Who, delete, Orig, _Json, _Layer]) ->
	Zones = lists:keydelete(Orig#test_zone.id, #test_zone.id, State#state.zones),
	Layer = lists:keyfind(Orig#test_zone.layer_id, #test_layer.id, State#state.layers),
	Zones = lists:delete(Orig#test_zone.id, Layer#test_layer.zones),
	Layer2 = Layer#test_layer{zones = Zones},
	Layers = lists:keyreplace(Layer2#test_layer.id, #test_layer.id, State#state.layers, Layer2),
	State#state{zones = Zones, layers = Layers}.

%% === auras =========================================================

auras_next_state(State, _Result, [_Transport, _Who, get, _Orig, _Json, _Layer]) ->
	State;

auras_next_state(State, _Result, [_Transport, _Who, put, undefined, _Json, undefined]) ->
	State;

auras_next_state(State, Result, [_Transport, _Who, put, undefined, Json, #test_layer{map_id = MapId} = Layer]) ->
	case zoneaura_put_valid(Json, undefined) of
		true ->
			BaseAura = extract_zoneaura(undefined, Result),
			Aura = BaseAura#test_zone{type = aura, map_id = MapId, layer_id = Layer#test_layer.id},
			Zones = State#state.zones ++ [Aura],
			Layer2 = Layer#test_layer{auras = Layer#test_layer.auras ++ [Aura#test_zone.id]},
			Layers = lists:keyreplace(Layer2#test_layer.id, #test_layer.id, State#state.layers, Layer2),
			State#state{layers = Layers, zones = Zones};
		false ->
			State
	end;

auras_next_state(State, _Result, [_Transport, _Who, put, undefined, _Json, _Layer]) ->
	State;

auras_next_state(State, Result, [_Transport, _Who, put, Original, Json, _Layer]) ->
	case zoneaura_put_valid(Json, Original) of
		true ->
			Aura = extract_zoneaura(Original, Result),
			Zones = lists:keyreplace(Aura#test_zone.id, #test_zone.id, State#state.zones, Aura),
			State#state{zones = Zones};
		false ->
			State
	end;

auras_next_state(State, _Result, [_Transport, _Who, delete, undefined, _Json, _Layer]) ->
	State;

auras_next_state(State, _Result, [_Transport, _Who, delete, Orignial, _Json, _Layer]) ->
	#test_zone{id = AuraId, layer_id = LayerId} = Orignial,
	Zones = lists:keydelete(AuraId, #test_zone.id, State#state.zones),
	Layer = lists:keyfind(LayerId, #test_layer.id, State#state.layers),
	AuraList = lists:delete(AuraId, Layer#test_layer.auras),
	Layer2 = Layer#test_layer{auras = AuraList},
	Layers = lists:keyreplace(Layer2#test_layer.id, #test_layer.id, State#state.layers, Layer2),
	State#state{zones = Zones, layers = Layers}.

%% === zones and auras ================================================

zoneaura_put_valid(undefined, Json) ->
	Name = proplists:get_value(<<"name">>, fix_json_keys(Json)),
	if
		Name == undefined ->
			false;
		Name == <<>> ->
			false;
		true ->
			true
	end;
zoneaura_put_valid(_,_) ->
	true.

extract_zoneaura(undefined, Result) ->
	#test_zone{
		id = {call, ?MODULE, extract_json, [Result, <<"id">>]},
		url = {call, ?MODULE, extract_json, [Result, <<"url">>]},
		properties = {call, ?MODULE, extract_json, [Result]}
	};
extract_zoneaura(Original, Result) ->
	Original#test_zone{
		properties = {call, ?MODULE, extract_json, [Result]}
	}.

%% === common ========================================================

extract_json({ok, Status, Heads, Body}) ->
	jsx:to_term(list_to_binary(Body));

extract_json({_SomeId, {ok, {text, Frame}}}) ->
	Json = jsx:to_term(Frame),
	proplists:get_value(<<"data">>, Json).

extract_json(Result, Key) when is_binary(Key) ->
	Json = extract_json(Result),
	proplists:get_value(Key, Json);

extract_json(Result, Keys) when is_list(Keys) ->
	Json = extract_json(Result),
	path_get(Keys, Json).

path_get([], Stuff) ->
	Stuff;
path_get([Key | Keys], Stuff) when is_integer(Key), is_list(Stuff) ->
	Stuff2 = lists:nth(Key, Stuff),
	path_get(Keys, Stuff2);
path_get([Key | Keys], Stuff) when is_list(Stuff) ->
	Stuff2 = proplists:get_value(Key, Stuff),
	path_get(Keys, Stuff2).

fix_json_keys(Json) ->
	lists:map(fun
		({Key, Val}) when is_atom(Key) ->
			{list_to_binary(atom_to_list(Key)), Val};
		(E) ->
			E
	end, Json).

%% ===================================================================
%% postconditions
%% ===================================================================

%% === maps ==========================================================



%maps_ws_post(_State, _Result, _Socket, _Who, _Action, _Map, _Json) ->
%	false.

%% === characters ==================================================

characters_http_post(_State, {ok, "422", _Head, _Body}, _Who, put, undefined, Json) ->
	case proplists:get_value(<<"name">>, Json) of
		undefined ->
			true;
		<<>> ->
			true;
		_ ->
			false
	end;

characters_http_post(_State, {ok, "405", _Head, _Body}, _Who, delete, undefined, _Json) ->
	true;

characters_http_post(State, {ok, "200", _Heads, Body}, Who, get, undefined, _Json) ->
	DecodedBody = jsx:to_term(list_to_binary(Body)),
	ExpectedBodies = [ P || #test_character{owner = O, properties = P} <- State#state.characters, O == Who],
	if
		length(DecodedBody) /= length(ExpectedBodies) ->
			?debugFmt("Different number of characters gotten", []),
			false;
		true ->
			Zipped = lists:zip(ExpectedBodies, DecodedBody),
			lists:all(fun({E,G}) ->
				assert_json(E, G)
			end, Zipped)
	end;

characters_http_post(_State, {ok, "201", Heads, Body}, _Who, put, undefined, Json) ->
	HasLocation = proplists:get_value("location", Heads) =/= undefined,
	GotBody = jsx:to_term(list_to_binary(Body)),
	HasLocation andalso assert_json(Json, GotBody).

characters_ws_post(_State, {ReplyTo, ReplyFrame}, _SocketTuple, _FakeWho, _Action, _Exists, _Json) ->
	assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, undefined).

%% === combatants ===================================================

combatants_http_post(_State, {ok, "201", Heads, Body}, _Who, put, undefined, Json, _Map) ->
	HasLocation = proplists:get_value("location", Heads) =/= undefined,
	GotBody = jsx:to_term(list_to_binary(Body)),
	HasLocation andalso assert_json(Json, GotBody);

combatants_http_post(_State, {ok, "401", Heads, _Body}, notauthed, _Action, _Exists, _Json, _Map) ->
	proplists:get_value("www-authenticate", Heads) =:= "persona";

combatants_http_post(_State, {ok, "403", _Heads, _Body}, notpartier, get, _Exists, _Json, _Map) ->
	true;

combatants_http_post(State, {ok, "200", _Heads, Body}, _Who, get, undefined, _Json, Map) ->
	GotBody = jsx:to_term(list_to_binary(Body)),
	Expecteds = lists:map(fun(Id) ->
		Combatant = lists:keyfind(Id, #test_combatant.id, State#state.combatants),
		Combatant#test_combatant.properties
	end, Map#test_map.combatants),
	if
		length(GotBody) =/= length(Expecteds) ->
			?debugMsg("not enough combatants gotten"),
			false;
		true ->
			lists:all(fun({E,G}) ->
				assert_json(E, G)
			end, lists:zip(Expecteds, GotBody))
	end;

combatants_http_post(_State, {ok, "405", _Heads, _Body}, _Who, delete, undefined, _Json, _Map) ->
	true.

combatants_ws_post(_State, {ReplyTo, ReplyFrame}, {_Socket, _Who, _MapId} , _FakeWho, delete, undefined, _Json, _Map) ->
	assert_ws_frame(ReplyFrame, reply, false, ReplyTo, undefined, undefined);

combatants_ws_post(State, {ReplyTo, ReplyFrame}, SocketTuple, _FakeWho, put, undefined, Json, _Map) ->
	case combatant_put_valid(Json, undefined) of
		false ->
			assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, undefined);
		true ->
			FirstFrame = assert_ws_frame(ReplyFrame, reply, true, reply, ReplyTo, udefined),
			{_Socket, _Who, MapId} = SocketTuple,
			FirstFrame andalso lists:all(fun
				({S, _W, Mid}) when Mid =:= MapId ->
					A = assert_ws_frame(S, put, undefined, combatants, undefined, Json),
					B = assert_ws_frame(S, put, undefined, map, Mid, undefined),
					A andalso B;
				(_) ->
					true
			end, State#state.ws)
	end.

%% === layers =======================================================

layers_http_post(_State, {ok, "201", Heads, Body}, Who, put, undefined, Json, #test_map{owner = Who}) ->
	HasLocation = proplists:get_value("location", Heads) =/= undefined,
	Got = jsx:to_term(list_to_binary(Body)),
	HasLocation andalso assert_json(Json, Got);

layers_http_post(_State, {ok, "403", _Heads, _Body}, _Who, put, undefined, _Json, _Map) ->
	true;

layers_http_post(_State, {ok, "200", _Heads, Body}, Who, put, _Layer, Json, #test_map{owner = Who}) ->
	GotBody = jsx:to_term(list_to_binary(Body)),
	assert_json(Json, GotBody);

layers_http_post(_State, {ok, "403", _Heads, _Body}, _Who, put, _Layer, _Json, _Map) ->
	true;

layers_http_post(_State, {ok, "200", _Heads, _Bdoy}, _Who, get, _Layer, _json, _Map) ->
	true;

layers_http_post(_State, {ok, "422", _Heads, _Body}, _Who, delete, undefined, _Json, _Map) ->
	true;

layers_http_post(State, {ok, "422", _Heads, _Body}, _Who, delete, Layer, _Json, _Map) ->
	MapId = Layer#test_layer.map_id,
	MapLayers = [L || #test_layer{map_id = Mid} = L <- State#state.layers, Mid =:= MapId],
	length(MapLayers) == 1;

layers_http_post(_State, {ok, "405", _Heads, _Body}, _Who, delete, undefined, _Json, _Map) ->
	true;

layers_http_post(State, {ok, "204", _Heads, _Body}, Who, delete, Layer, _Json, _Map) ->
	Map = lists:keyfind(Layer#test_layer.map_id, #test_map.id, State#state.maps),
	Map#test_map.owner =:= Who;

layers_http_post(State, {ok, "403", _Heads, _Body}, Who, delete, Layer, _Json, _Map) ->
	Map = lists:keyfind(Layer#test_layer.map_id, #test_map.id, State#state.maps),
	Map#test_map.owner =/= Who.

%% === zones ========================================================

zones_http_post(_State, {ok, "403", _Head, _Body}, notpartier, _Action, _Exists, _Json, _Layer) ->
	true;

zones_http_post(_State, {ok, "405", _Head, _Body}, _Who, delete, undefined, _Json, _Layer) ->
	true;

zones_http_post(State, {ok, "201", Heads, Body}, Who, put, undefined, Json, Layer) ->
	Map = lists:keyfind(Layer#test_layer.map_id, #test_map.id, State#state.maps),
	HasLocation = proplists:get_value("location", Heads) =/= undefined,
	GotBody = jsx:to_term(list_to_binary(Body)),
	HasLocation andalso assert_json(Json, GotBody);

zones_http_post(State, {ok, "200", _Heads, Body}, _Who, get, undefined, _Json, Layer) ->
	Expected = lists:map(fun(Id) ->
		Zone = lists:keyfind(Id, #test_zone.id, State#state.zones),
		Zone#test_zone.properties
	end, Layer#test_layer.zones),
	Got = jsx:to_term(list_to_binary(Body)),
	lists:all(fun({E,G}) ->
		assert_json(E,G)
	end, lists:zip(Expected, Got));

zones_http_post(_State, {ok, "200", _Heads, Body}, _Who, get, Zone, _Json, _Layer) ->
	Got = jsx:to_term(list_to_binary(Body)),
	assert_json(Zone#test_zone.properties, Got).

%% === auras ========================================================

auras_http_post(_State, {ok, "404", _Heads, _Body}, _Who, delete, undefined, _Json, undefined) ->
	true;

auras_http_post(_State, {ok, "404", _Heads, _Body}, _Who, get, undefined, _Json, undefined) ->
	true;

auras_http_post(_State, {ok, "405", _Heads, _Body}, _Who, delete, undefined, _Json, _Layer) ->
	true;

auras_http_post(_State, {ok, "201", Heads, Body}, _Who, put, undefined, Json, _Layer) ->
	HasLocation = proplists:get_value("location", Heads) =/= undefined,
	GotBody = jsx:to_term(list_to_binary(Body)),
	HasLocation andalso assert_json(Json, GotBody);

auras_http_post(State, {ok, "200", _Heads, Body}, _Who, get, undefined, _Json, Layer) ->
	GotBody = jsx:to_term(list_to_binary(Body)),
	if
		length(GotBody) =/= length(Layer#test_layer.auras) ->
			?debugMsg("got and expected number of auras don't match"),
			false;
		true ->
			Expected = lists:map(fun(Id) ->
				Aura = lists:keyfind(Id, #test_zone.id, State#state.zones),
				Aura#test_zone.properties
			end, Layer#test_layer.auras),
			lists:all(fun({E,G}) ->
				assert_json(E,G)
			end, lists:zip(Expected, GotBody))
	end.

%% === generic postcondtiion ========================================

postcondition(_State, {call, _Mod, connect_ws, [notpartier | _]}, {error, {403, _}}) ->
	true;

postcondition(_State, {call, _Mod, connect_ws, [notauthed | _]}, {error, {401, _}}) ->
	true;

postcondition(_State, {call, _Mod, connect_ws, [Who, Map]}, {error, Wut}) ->
	?debugFmt("Error connectiong to websocket.~n"
		"    Who: ~p~n"
		"    MapId: ~p~n"
		"    Error: ~p", [Who, Map#test_map.id, Wut]),
	false;

postcondition(_State, {call, _Mod, connect_ws, _Args}, _Result) ->
	true;

postcondition(_State, {call, _Mod, disconnect_ws, _Args}, ok) ->
	true;

postcondition(State, {call, Mod, _Func, _Args} = Call, Result) ->
	Mod:postcondition(State, Call, Result);

%postcondition(State, {call, _Mod, Cmd, [Mode | Args]}, Result) ->
%	CmdStr = atom_to_list(Cmd),
%	{FunStr, ArgsList} = case Mode of
%		http ->
%			{CmdStr ++ "_http_post", [State, Result | Args]};
%		_ ->
%			{CmdStr ++ "_ws_post", [State, Result, Mode | Args]}
%	end,
%	Fun = list_to_atom(FunStr),
%	erlang:apply(?MODULE, Fun, ArgsList);

%postcondition(_State, {call, _Mod, Resource, [http | _] = Args}, {ok, "404", _Heads, _Body}) ->
%	should_404(Resource, Args);
%
%postcondition(_State, {call, _Mod, Resource, [http | _] = Args}, {ok, "405", _Heads, _Body}) ->
%	should_405(Resource, Args);
%
%postcondition(_State, {call, _Mod, _Resource, [http, notpartier | _] = _Args}, {ok, "200", _Heads, _Body}) ->
%	false;
%
%postcondition(_State, {call, _Mod, _Resource, [http, notauthed | _]}, {ok, "401", Heads, _Body}) ->
%	case proplists:get_value("www-authenticate", Heads) of
%		"persona" ->
%			true;
%		Other ->
%			?debugFmt("www-authenticate header was ~s; not persona", [Other]),
%			false
%	end;
%
%postcondition(_State, {call, _Mod, _Resource, [http, notpartier | _]}, {ok, "401", Heads, _Body}) ->
%	case proplists:get_value("www-authenticate", Heads) of
%		"persona" ->
%			true;
%		Other ->
%			?debugFmt("www-authenticate header was ~s; not persona", [Other]),
%			false
%	end;
%
%postcondition(State, {call, _Mod, Resource, [http, _Who, get | _] = Args}, {ok, "200", _Heads, Body}) ->
%	DecodedBody = jsx:to_term(list_to_binary(Body)),
%	ExpectedBody = expected_body(State, Resource, Args),
%	assert_json(ExpectedBody, DecodedBody);
%
%postcondition(_State, {call, _Mod, maps, [http, _Who, put, undefined, PutBody]}, {ok, "201", Headers, Body}) ->
%	HasLocation = proplists:get_value("location", Headers) =/= undefined,
%	ExpectedBody = jsx:to_term(jsx:to_json(PutBody)),
%	DecodedBody = jsx:to_term(list_to_binary(Body)),
%	assert_json(ExpectedBody, DecodedBody) andalso HasLocation;
%
%postcondition(State, {call, _Mod, maps, [http, _Who, put, Map, PutBody] = Args}, {ok, "200", Headers, Body}) ->
%	DecodedBody = jsx:to_term(list_to_binary(Body)),
%	ExpectedBody = expected_body(State, Map, Args),
%	assert_json(ExpectedBody, DecodedBody);
%
%postcondition(_State, {call, _Mod, characters, [http, _Who, put, undefined, PutBody]}, {ok, "201", Headers, Body}) ->
%	HasLocation = proplists:get_value("location", Headers) =/= undefined,
%	ExpectedBody = jsx:to_term(jsx:to_json(PutBody)),
%	DecodedBody = jsx:to_term(list_to_binary(Body)),
%	assert_json(ExpectedBody, DecodedBody) andalso HasLocation;
%
%postcondition(_State, {call, _Mod, Resource, [http, _Who, put, undefined, PutBody, Map]}, {ok, "201", Headers, Body}) ->
%	HasLocation = proplists:get_value("location", Headers) =/= undefined,
%	ExpectedBody = jsx:to_term(jsx:to_json(PutBody)),
%	DecodedBody = jsx:to_term(list_to_binary(Body)),
%	assert_json(ExpectedBody, DecodedBody) andalso HasLocation;
%
%postcondition(_State, {call, _Mod, Resource, [http, _Who, put, undefined | _] = Args}, {ok, "422", _Head, _Body}) ->
%	PutBody = extract_req_body(Resource, Args),
%	case proplists:get_value(name, PutBody) of
%		undefined ->
%			true;
%		<<>> ->
%			true;
%		_ ->
%			?debugFmt("should not have gotten a 422 on put~n"
%				"    Resouce: ~p~n"
%				"    Args: ~p~n", [Resource, Args]),
%			false
%	end;
%
%postcondition(_State, {call, _Mod, maps, [http, Who, delete, #test_map{owner = Who} | _]}, {ok, "204", _Head, _Body}) ->
%	true;
%
%postcondition(_State, {call, _Mod, maps, [http, _Who, delete, #test_map{owner = _NotWhoe} | _]}, {ok, "403", _Head, _Body}) ->
%	true;
%
%postcondition(_State, {call, _Mod, _Resource, [http, _Who, delete, Exists | _]}, {ok, "204", _Heads, _Body}) when Exists =/= undefined ->
%	true;
%
%postcondition(_State, {call, _Mod, connect_ws, [notpartier | _]}, {error, {403, _}}) ->
%	true;
%
%postcondition(_State, {call, _Mod, connect_ws, [notauthed | _]}, {error, {401, _}}) ->
%	true;
%
%postcondition(_State, {call, _Mod, connect_ws, [Who, Map]}, {error, Wut}) ->
%	?debugFmt("Error connectiong to websocket.~n"
%		"    Who: ~p~n"
%		"    MapId: ~p~n"
%		"    Error: ~p", [Who, Map#test_map.id, Wut]),
%	false;
%
%postcondition(_State, {call, _Mod, connect_ws, _Args}, _Result) ->
%	true;
%
%postcondition(_State, {call, _Mod, disconnect_ws, _Args}, ok) ->
%	true;
%
%postcondition(State, {call, _Mod, Cmd, [Ws, Who, Action, Exists, Json | ArgsRest]}, Result) ->
%	{TestedSocket, _, _} = Ws,
%	case gen_websocket:recv(TestedSocket, 5000) of
%		{ok, {text, Frame}} ->
%			DecodedFrame = jsx:to_term(Frame),
%			case proplists:get_value(<<"reply_to">>, DecodedFrame) of
%				Result ->
%					Accepted = proplists:get_value(<<"accepted">>, DecodedFrame),
%					Data = proplists:get_value(<<"data">>, DecodedFrame),
%					ws_postcondition(State, Cmd, Who, Action, Exists, Json, ArgsRest, Accepted, Data);
%				NotResult ->
%					?debugFmt("incorrect reply indicator: ~p", [NotResult]),
%					false
%			end;
%		Else ->
%			?debugFmt("Did not get an initial frame: ~p", [Else]),
%			false
%	end;

postcondition(State, Call, Result) ->
	?debugFmt("Postcondition fall through:~n"
		"State: ~p~n"
		"Call: ~p~n"
		"Result: ~p", [State, Call, Result]),
	false.

assert_ws_frame({ok, Frame}, Action, Accepted, Type, TypeId, Data) ->
	assert_ws_frame(Frame, Action, Accepted, Type, TypeId, Data);

assert_ws_frame({text, Frame}, Action, Accepted, Type, TypeId, Data) ->
	Got = jsx:to_term(Frame),
	GotData = proplists:get_value(<<"data">>, Got),
	Expected = build_expected_ws_frame(Action, Accepted, Type, TypeId),
	assert_json(Expected, Got) andalso assert_json(Data, GotData);

assert_ws_frame(Socket, Action, Accepted, Type, TypeId, Data) when is_pid(Socket) ->
	ReplyFrame = gen_websocket:recv(Socket, 1000),
	assert_ws_frame(ReplyFrame, Action, Accepted, Type, TypeId, Data);

assert_ws_frame(Else, _Action, _Accepted, _Type, _TypeId, _Data) ->
	?debugFmt("Did not get a frame from a socket: ~p", [Else]),
	false.

build_expected_ws_frame(Action, Accepted, Type, TypeId) ->
	Fun = fun
		({action, Act}, Acc) ->
			[{<<"action">>, list_to_binary(atom_to_list(Act))} | Acc];
		({accepted, undefined}, Acc) ->
			Acc;
		({accepted, Bool}, Acc) ->
			[{<<"accepted">>, Bool} | Acc];
		({type, undefined}, Acc) ->
			Acc;
		({type, T}, Acc) ->
			[{<<"type">>, list_to_binary(atom_to_list(T))} | Acc];
		({typeid, undefined}, Acc) ->
			Acc;
		({typeid, Id}, Acc) ->
			[{<<"type_id">>, Id} | Acc]
	end,
	lists:foldl(Fun, [], [{action, Action}, {accepted, Accepted}, {type, Type}, {typeid, TypeId}]).


%assert_evelope({text, FrameTxt}, ReplyTo, Accepted, Type, Data) ->
%	Frame = jsx:to_term(FrameTxt),
%	Expected = [{<<"reply_to">>, ReplyTo},{<<"type">>, Type},{<<"accepted">>, Accepted}, {<<"data">>, Data}]j,
%	assert_json(Expected, Frame).

extract_req_body(characters, [_, _, _, _, Json]) ->
	Json;
extract_req_body(maps, [_, _, _, _, Json]) ->
	Json.

expected_body(State, maps, [http, _Who, get, undefined | _]) ->
	lists:map(fun(M) ->
		M#test_map.properties
	end, State#state.maps);

expected_body(_State, maps, [http, _Who, get, Map | _]) ->
	Map#test_map.properties;

expected_body(State, characters, [http, _Who, get, undefined | _]) ->
	lists:map(fun(C) ->
		C#test_character.properties
	end, State#state.characters);

expected_body(State, combatants, [http, _Who, get, undefined, _Json, Map]) ->
	lists:map(fun(CombatantId) ->
		Combatant = lists:keyfind(CombatantId, #test_combatant.id, State#state.combatants),
		Combatant#test_combatant.properties
	end, Map#test_map.combatants).

assert_json(Expected, Expected) ->
	true;
assert_json([{}], Got) ->
	assert_json([], Got);
assert_json(Expected, [{}]) ->
	assert_json(Expected, []);
assert_json(undefined, Got) ->
	?debugFmt("Expectd nothing, got ~p", [Got]),
	false;
assert_json(Expected, undefined) ->
	?debugFmt("Expected data, got undefined: ~p", [Expected]),
	false;
assert_json(Expected, Got) ->
	Expected1 = fix_json_keys(Expected),
	Got1 = fix_json_keys(Got),
	assert_json(lists:sort(Expected1), lists:sort(Got1), []).

assert_json([], _Got, []) ->
	true;

assert_json([], _Got, Err) ->
	?debugFmt("assert json failure: ~p", [Err]),
	false;

assert_json(Expected, [], Err) ->
	Err2 = Err ++ [{K, V, undefined} || {K, V} <- Expected],
	assert_json([], [], Err2);

assert_json([{Key, Value} | E], [{Key, Value} | G], Err) ->
	assert_json(E, G, Err);

assert_json([{Key, Value} | E], [{Key, OVal} | G], Err) ->
	assert_json(E, G, [{Key, Value, OVal} | Err]);

assert_json([{Key, _V} | _] = E, [{SmallKey, _} | G], Err) when SmallKey < Key ->
	assert_json(E, G, Err);

assert_json([{Key, V} | E], G, Err) ->
	assert_json(E, G, [{Key, V, undefined} | Err]).

should_404(characters, [_, _, GetOrDelete, undefined | _]) when GetOrDelete == get; GetOrDelete == delete ->
	true;

should_404(characters, _Args) ->
	false;

should_404(maps, _) ->
	false;

should_404(ZoneAura, [_, _, _, _, undefined | _]) when ZoneAura == zones; ZoneAura == auras ->
	true;

should_404(_Whatevs, [_, _, _, undefined | _]) ->
	true;

should_404(_, _) ->
	false.

should_405(_Whatevs, [_, _, delete, undefined | _]) ->
	true;

should_405(_, _) ->
	false.

%% ===================================================================
%% commands
%% ===================================================================

characters(Transport, Who, Action, Existant, Json) ->
	send_request(characters, Transport, Who, Action, Existant, Json, []).

combatants(Transport, Who, Action, Existant, Json, Map) ->
	send_request(combatants, Transport, Who, Action, Existant, Json, [{map, Map}]).

layers(Transport, Who, Action, Existant, Json, Map) ->
	send_request(layers, Transport, Who, Action, Existant, Json, [{map, Map}]).

zones(Transport, Who, Action, Zone, Json, Layer) ->
	Map = #test_map{id = Layer#test_layer.id, url = list_to_binary("http://localhost:10001/maps/" ++ integer_to_list(Layer#test_layer.map_id))},
	send_request(zones, Transport, Who, Action, Zone, Json, [{map, Map}, {layer, Layer}]).

auras(Transport, Who, Action, Aura, Json, Layer) ->
	Map = #test_map{id = Layer#test_layer.id, url = list_to_binary("http://localhost:10001/maps/" ++ integer_to_list(Layer#test_layer.map_id))},
	send_request(auras, Transport, Who, Action, Aura, Json, [{map, Map}, {layer, Layer}]).

connect_ws(Who, Map) ->
	Cookie = list_to_binary(cookie(Who)),
	Url = binary_to_list(Map#test_map.ws_url),
	SocketOpts = [{owner_exit, {shutdown, normal}},
		{headers, [{<<"Cookie">>, Cookie}]}],
	case gen_websocket:connect(Url,SocketOpts) of
		{ok, Socket} ->
			Socket;
		Else ->
			?debugFmt("Could not connect socket: ~p~n"
				"    Url: ~p~n"
				"    Cookie: ~p", [Else, Url, Cookie]),
			Else
	end.

disconnect_ws({Ws, _Who, _MapId}) ->
	gen_websocket:shutdown(Ws, normal).

%% ===================================================================
%% internal
%% ===================================================================

lists_pos(Elem, List) ->
	PosFun = fun(E) ->
		E =/= Elem
	end,
	case lists:splitwith(PosFun, List) of
		{Length, [_Elem | _Tail]} ->
			length(Length) + 1;
		_ ->
			0
	end.

cookie(NameAtom) ->
	rpgb_test_util:make_cookie(<<"rpgbsid">>, list_to_binary(atom_to_list(NameAtom) ++ "_session")).

send_request(What, http, Who, Action, Existant, Json, Args) ->
	Headers = ?headers([?cookie(Who)]),
	Url = url_from_rec(What, Existant, Args),
	ibrowse_req(binary_to_list(Url), Headers, Action, Json);

send_request(What, Sockets, Who, Action, Existant, Json, Args) when is_list(Sockets) ->
	Socket = proplists:get_value(Who, Sockets),
	send_request(What, Socket, Who, Action, Existant, Json, Args);

send_request(_What, undefined, _Who, _, _, _, _) ->
	{error, no_socket};

send_request(What, {Socket, _RealWho, _MapId}, _Who, Action, Existant, Json, _Args) ->
	Frame = build_ws_frame(What, Action, Existant, Json),
	Json2 = jsx:to_json(Frame),
	ok = gen_websocket:send(Socket, Json2),
	Reply = proplists:get_value(<<"reply_with">>, Frame),
	Return = gen_websocket:recv(Socket, 1000),
	{Reply, Return}.

url_from_rec(maps, undefined, _Args) ->
	<<"http://localhost:10001/maps">>;

url_from_rec(characters, undefined, _Args) ->
	<<"http://localhost:10001/characters">>;

url_from_rec(ZoneOrAura, undefined, Args) when ZoneOrAura =:= zones; ZoneOrAura =:= auras ->
	#test_layer{url = LayerUrl} = proplists:get_value(layer, Args),
	iolist_to_binary(io_lib:format("~s/~s", [LayerUrl, ZoneOrAura]));

url_from_rec(Type, undefined, Args) ->
	#test_map{url = Murl} = proplists:get_value(map, Args),
	iolist_to_binary(io_lib:format("~s/~s", [Murl, Type]));

url_from_rec(maps, Rec, _Args) ->
	Rec#test_map.url;

url_from_rec(characters, Rec, _Args) ->
	Rec#test_character.url;

url_from_rec(combatants, Rec, _Args) ->
	Rec#test_combatant.url;

url_from_rec(layers, Rec, _Args) ->
	Rec#test_layer.url;

url_from_rec(_ZoneOrAura, Rec, _Args) ->
	Rec#test_zone.url.

build_ws_frame(TypePlural, Action, Existant, Json) ->
	Type = ws_depluraize(TypePlural),
	Payload = [
		{<<"reply_with">>, random:uniform(1000)},
		{<<"action">>, list_to_binary(atom_to_list(Action))},
		{<<"type">>, Type}
	],
	Payload1 = maybe_add_id(Payload, Existant),
	maybe_add_data(Payload1, Action, Json).

maybe_add_id(Payload, undefined) ->
	Payload;
maybe_add_id(Payload, #test_map{id = Id}) ->
	[{<<"id">>, Id} | Payload];
maybe_add_id(Payload, #test_layer{id = Id}) ->
	[{<<"id">>, Id} | Payload];
maybe_add_id(Payload, Thing) when is_list(Thing) ->
	Id = proplists:get_value(<<"id">>, Thing),
	[{<<"id">>, Id} | Payload];
maybe_add_id(Payload, Thing) when is_integer(Thing) ->
	[{<<"id">>, Thing} | Payload].

maybe_add_data(Payload, Action, _Json) when Action =:= get; Action =:= delete ->
	Payload;
maybe_add_data(Payload, _Action, Json) ->
	[{<<"data">>, Json} | Payload].

ws_depluraize(Atom) ->
	AtomStr = atom_to_list(Atom),
	Len = length(AtomStr),
	AtomStr1 = lists:sublist(AtomStr, Len - 1),
	list_to_binary(AtomStr1).

ibrowse_req(Url, Headers, Action, Json) when is_binary(Url) ->
	ibrowse_req(binary_to_list(Url), Headers, Action, Json);

ibrowse_req(Url, Headers, Action, _Json) when Action =:= get; Action =:= delete ->
	ibrowse:send_req(Url, Headers, Action);

ibrowse_req(Url, Headers, Action, Json) ->
	Binary = jsx:to_json(Json),
	ibrowse:send_req(Url, Headers, Action, Binary).

proplists_get(Keys, Props) ->
	Folder = fun
		(Key, Acc) when is_list(Acc) ->
			proplists:get_value(Key, Acc);
		(_Key, _) ->
			undefined
	end,
	lists:foldl(Folder, Props, Keys).

get_url(Path) ->
	MapFun = fun
		(E) when is_atom(E) ->
			atom_to_list(E);
		(E) when is_integer(E) ->
			integer_to_list(E);
		(E) ->
			E
	end,
	binary_to_list(rpgb:get_url(lists:map(MapFun, Path))).

get_sessions() ->
	Whos = [owner, partier1, partier2, notpartier],
	lists:map(fun(Who) ->
		SessionId = <<(list_to_binary(atom_to_list(Who)))/binary, "_session">>,
		{ok, Session} = rpgb_session:get(SessionId),
		{Who, Session}
	end, Whos).
