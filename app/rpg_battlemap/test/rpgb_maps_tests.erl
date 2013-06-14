-module(rpgb_maps_tests).

-include("prop_tests.hrl").

simple_args(http, [Who, Action, undefined, _Json]) ->
	[Who, Action, undefined];
simple_args(http, [Who, Action, #test_map{id = Id}, _Json]) ->
	[Who, Action, Id];
simple_args(websocket, [{_Socket, Who, MapId}, Action, _Json]) ->
	[Who, Action, MapId].

%% === commands ============================================

http(Who, Action, Map, Json) ->
	Sessions = rpgb_requests_tests:get_sessions(),
	Ids = lists:foldl(fun({SessWho, Session}, Acc) ->
		if
			SessWho =:= Who ->
				Acc;
			SessWho =:= notpartier ->
				Acc;
			true ->
				User = rpgb_session:get_user(Session),
				[User#rpgb_rec_user.id | Acc]
		end
	end, [], Sessions),
	Json2 = case {Map, Json} of
		{undefined, [{}]} ->
			?debugFmt("putting id's as participants: ~p", [Ids]),
			[{<<"participant_ids">>, Ids}];
		{undefined, _} ->
			?debugFmt("putting id's as participants: ~p", [Ids]),
			[{<<"participant_ids">>, Ids} | Json];
		_ ->
			Json
	end,
	?debugFmt("der json: ~p", [Json2]),
	rpgb_requests_tests:send_request(maps, http, Who, Action, Map, Json2, []).

websocket({_SockRef, Who, MapId} = Socket, Action, Json) ->
	rpgb_requests_tests:send_request(maps, Socket, undefined, Action, MapId, Json, []).

%% === preconditions =======================================

precondition(State, {call, ?MODULE, websocket, [undefined | _]}) ->
	false;
precondition(State, {call, ?MODULE, http, [notpartier, put, undefined, _Json]}) ->
	false;
precondition(_State, _Call) ->
	true.

%% === next_state ==========================================

next_state(State, _Result, {call, ?MODULE, http, [_Who, get | _]}) ->
	State;

next_state(State, _Result, {call, ?MODULE, http, [notauthed | _]}) ->
	State;

next_state(State, Result, {call, ?MODULE, http, [Who, put, undefined, Json]}) ->
	case map_put_valid(Json, undefined) of
		true ->
			TestMap = extract_map(undefined, Who, Result),
			TestLayer = rpgb_layers_tests:extract_layer(undefined, Who, {map, Result}),
			TestLayer1 = TestLayer#test_layer{map_id = TestMap#test_map.id},
			Maps = State#state.maps ++ [TestMap],
			Layers = State#state.layers ++ [TestLayer1],
			State#state{maps = Maps, layers = Layers};
		false ->
			State
	end;

next_state(State, Result, {call, ?MODULE, http, [Who, put, #test_map{owner = Who} = Original, Json]}) ->
	case map_put_valid(Json, Original) of
		true ->
			NewMap = extract_map(Original, Who, Result),
			Maps = lists:keyreplace(Original#test_map.id, #test_map.id, State#state.maps, NewMap),
			State#state{maps = Maps};
		false ->
			State
	end;

next_state(State, _Result, {call, ?MODULE, http, [_Who, put, _Map, _Json]}) ->
	State;

next_state(State, _Result, {call, ?MODULE, http, [_Who, delete, undefined, _Json]}) ->
	State;

next_state(State, _Result, {call, ?MODULE, http, [Who, delete, #test_map{owner = Who} = Map, _Json]}) ->
	Maps = lists:keydelete(Map#test_map.id, #test_map.id, State#state.maps),
	LayerIds = [Id || #test_layer{id = Id, map_id = Mid} <- State#state.layers, Mid =:= Map#test_map.id],
	Layers = lists:filter(fun(L) ->
		not lists:member(L#test_layer.id, LayerIds)
	end, State#state.layers),
	Zones = lists:filter(fun(Z) ->
		not lists:member(Z#test_zone.layer_id, LayerIds)
	end, State#state.zones),
	Combatants = lists:filter(fun(C) ->
		C#test_combatant.map_id =/= Map#test_map.id
	end, State#state.combatants),
	Sockets = lists:filter(fun({S, _W, MapId}) ->
		case Map#test_map.id of
			MapId ->
				catch gen_websocket:shutdown(S, normal),
				false;
			_ ->
				true
		end
	end, State#state.ws),
	State#state{maps = Maps, layers = Layers, zones = Zones, combatants = Combatants, ws = Sockets};

next_state(State, _Result, {call, ?MODULE, http, [_Who, delete, _Map, _Json]}) ->
	State;

next_state(State, Result, {call, ?MODULE, websocket, [{Socket, Who, MapId}, put, Json]}) ->
	Map = lists:keyfind(MapId, #test_map.id, State#state.maps),
	case Map of
		#test_map{owner = Who} ->
			Map2 = extract_map(Map, Who, Result),
			Maps = lists:keyreplace(MapId, #test_map.id, State#state.maps, Map2),
			State#state{maps = Maps};
		false ->
			Ws = lists:keydelete(Socket, 1, State#state.ws),
			State#state{ws = Ws};
		_NotWho ->
			State
	end;

next_state(State, _Result, {call, ?MODULE, websocket, _Args}) ->
	State.

map_put_valid(Json, undefined) ->
	Name = proplists:get_value(<<"name">>, rpgb_requests_tests:fix_json_keys(Json)),
	if
		Name =:= undefined ->
			false;
		Name =:= <<>> ->
			false;
		true ->
			true
	end;
map_put_valid(_,_) ->
	true.

extract_map(Original, Who, Result) ->
	M1 = case Original of
		undefined ->
			#test_map{id = {call, rpgb_requests_tests, extract_json, [Result, <<"id">>]},
				url = {call, rpgb_requests_tests, extract_json, [Result, <<"url">>]},
				ws_url = {call, rpgb_requests_tests, extract_json, [Result, <<"websocketUrl">>]}, owner = Who};
			_ ->
				Original
	end,
	M1#test_map{
		properties = {call, rpgb_requests_tests, extract_json, [Result]}
	}.

%% === postcondition =======================================

postcondition(_State, {call, ?MODULE, http, [_Who, _Action, _Map, _Json]}, {ok, "404", _Heads, _Body}) ->
	false;

postcondition(_State, {call, ?MODULE, http, [_Who, delete, undefined, _Json]}, {ok, "405", _Heads, _Body}) ->
	true;

postcondition(_State, {call, ?MODULE, http, [Who, _Action, #test_map{owner = Who}, _Json]}, {ok, "405", _Heads, _Body}) ->
	false; 

postcondition(_State, {call, ?MODULE, http, [_Who, put, undefined, Json]}, {ok, "422", _Head, _Body}) ->
	case proplists:get_value(name, Json) of
		undefined ->
			true;
		<<>> ->
			true;
		_ ->
			false
	end;

postcondition(_State, {call, ?MODULE, http, [notauthed, _Action, _Map, _Json]}, {ok, "401", Heads, _Body}) ->
	proplists:get_value("www-authenticate", Heads) =:= "persona";

postcondition(_State, {call, ?MODULE, http, [notpartier, _Action, _Map, _Json]}, {ok, "403", _Heads, Body}) ->
	true;

postcondition(_State, {call, ?MODULE, http, [_Who, put, undefined, Json]}, {ok, "201", Headers, Body}) ->
	HasLocation = proplists:get_value("location", Headers) =/= undefined,
	ExpectedBody = jsx:to_term(jsx:to_json(Json)),
	DecodedBody = jsx:to_term(list_to_binary(Body)),
	rpgb_requests_tests:assert_json(ExpectedBody, DecodedBody) andalso HasLocation;

postcondition(State, {call, ?MODULE, http, [Who, put, #test_map{owner = Who, id = MapId}, Json]}, {ok, "200", _Headers, Body}) ->
	PostedBody = jsx:to_term(jsx:to_json(Json)),
	DecodedBody = jsx:to_term(list_to_binary(Body)),
	JsonOkay = rpgb_requests_tests:assert_json(PostedBody, DecodedBody),
	JsonOkay andalso lists:all(fun
		({Socket, _SomeWho, Mid}) when Mid =:= MapId ->
			rpgb_requests_tests:assert_ws_frame(Socket, put, undefined, map, MapId, Json);
		(_) ->
			true
	end, State#state.ws);

postcondition(State, {call, ?MODULE, http, [Who, get, undefined, _Json]}, {ok, "200", _Headers, Body}) ->
	GotBody = jsx:to_term(list_to_binary(Body)),
	Expected = [P || #test_map{owner = O, properties = P} <- State#state.maps, O == Who],
	GotEnough = length(GotBody) =:= length(Expected),
	GotEnough andalso lists:all(fun(Json) ->
		Id = proplists:get_value(<<"id">>, Json),
		case lists:keyfind(Id, #test_map.id, State#state.maps) of
			false ->
				false;
			Map ->
				rpgb_requests_tests:assert_json(Map#test_map.properties, Json)
		end
	end, GotBody);

postcondition(_State, {call, ?MODULE, http, [_Who, put, _Map, _Json]}, {ok, "403", _Headers, _Body}) ->
	true;

postcondition(_State, {call, ?MODULE, http, [_Who, get, Map, _Json]}, {ok, "200", _Headers, Body}) ->
	GotBody = jsx:to_term(list_to_binary(Body)),
	rpgb_requests_tests:assert_json(Map#test_map.properties, GotBody);

postcondition(State, {call, ?MODULE, http, [Who, delete, #test_map{owner = Who, id = MapId}, _Json]}, {ok, "204", _Headers, _Body}) ->
	lists:all(fun
		({S, _SomeWho, Mid}) when MapId =:= Mid ->
			rpgb_requests_tests:assert_ws_frame(S, delete, undefined, map, MapId, undefined);
		(_) ->
			true
	end, State#state.ws);

postcondition(_State, {call, ?MODULE, http, [_Who, delete, _Map, _Json]}, {ok, "403", _Headers, _Body}) ->
	true;

postcondition(State, {call, ?MODULE, websocket, _Args}, {_Reply, {error, Wut}}) ->
	?debugFmt("error on first reply for websocket: ~p", [Wut]),
	false;

postcondition(State, {call, ?MODULE, websocket, [{Socket, _Who, MapId}, get, _Json]}, {ReplyTo, ReplyFrame}) ->
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			ErrorFrame = rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, undefined),
			Next = gen_websocket:recv(Socket, 1000),
			ErrorFrame andalso Next == {error, timeout};
		Map ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ReplyTo, Map#test_map.properties)
	end;

postcondition(_State, {call, ?MODULE, websocket, [_Socket, delete, _Json]}, {ReplyTo, ReplyFrame}) ->
	rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, undefined);

%postcondition(State, {call, ?MODULE, websocket, [{Socket, _Who, MapId}, Action, Json]}, {ReplyTo, ReplyFrame}) ->
%	ReplyFrameOkay = rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ReplyTo, Json),
%	OtherSockets = [Other || {_S, _W, Mid} = Other <- State#state.ws, Mid =:= MapId],
%	Others = lists:all(fun({S,_W,_M}) ->
%		rpgb_requests_tests:assert_ws_frame(S, put, undefined, maps, MapId, Json)
%	end, OtherSockets),
%	ReplyFrameOkay andalso Others;

postcondition(State, {call, ?MODULE, websocket, [{Socket, Who, MapId}, put, Json]}, {ReplyTo, ReplyFrame}) ->
	?debugFmt("The reply to is ~p", [ReplyTo]),
	Map = lists:keyfind(MapId, #test_map.id, State#state.maps),
	case Map of
		false ->
			?debugFmt("Could not find map ~p", [MapId]),
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, <<"not owner">>),
			Closed = gen_websocket:recv(Socket, 1000),
			Closed == {error, closed};
		#test_map{owner = Who} ->
			FirstFrame = rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ReplyTo, Json),
			AllOthers = lists:all(fun
				({S, _W, Mid} = Sdata) when Mid =:= MapId ->
					?debugFmt("dealing with socket ~p", [Sdata]),
					rpgb_requests_tests:assert_ws_frame(S, put, undefined, map, MapId, Json);
				(_) ->
					true
			end, State#state.ws),
			FirstFrame andalso AllOthers;
		_NotWho ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, <<"not owner">>)
	end.
