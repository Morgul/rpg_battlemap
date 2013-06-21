-module(rpgb_maps_tests).

-include("prop_tests.hrl").

simple_args(http, [Who, Action, undefined, _Json, Name]) ->
	[Who, Action, undefined, Name];
simple_args(http, [Who, Action, #test_map{id = Id}, _Json, Name]) ->
	[Who, Action, Id, Name];
simple_args(websocket, [{_Socket, Who, MapId}, Action, _Json, Name]) ->
	[Who, Action, MapId, Name].

command(#state{maps = []}) ->
	[{call, ?MODULE, http, [rpgb_requests_tests:g_who(), post, undefined, rpgb_prop:g_mapjson(), rpgb_prop:g_name()]}];
command(State) ->
	[{call, ?MODULE, http, [rpgb_requests_tests:g_who(), rpgb_requests_tests:g_action(), rpgb_requests_tests:g_maybe_exists(State#state.maps), rpgb_prop:g_mapjson(), g_maybe_name()]},
	{call, ?MODULE, websocket, [rpgb_requests_tests:g_maybe_exists(State#state.ws), rpgb_requests_tests:g_action(), rpgb_prop:g_mapjson(), g_maybe_name()]}].

g_maybe_name() ->
	oneof([undefined, null, <<>>, rpgb_prop:g_name()]).

%% === commands ============================================

http(Who, Action, Map, Json, Name) ->
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
	Json3 = maybe_set_name(Json2, Name),
	?debugFmt("der json: ~p", [Json3]),
	rpgb_requests_tests:send_request(maps, http, Who, Action, Map, Json3, []).

websocket({_SockRef, _Who, MapId} = Socket, Action, Json, Name) ->
	Json2 = maybe_set_name(Json, Name),
	rpgb_requests_tests:send_request(maps, Socket, undefined, Action, MapId, Json2, []).

maybe_set_name([{}], undefined) ->
	[{}];

maybe_set_name([{}], Name) ->
	[{<<"name">>, Name}];

maybe_set_name(Json,undefined) ->
	lists:keydelete(<<"name">>, 1, Json);

maybe_set_name(Json, Name) ->
	[{name, Name} | lists:keydelete(<<"name">>, 1, Json)].

%% === preconditions =======================================

precondition(_State, {call, ?MODULE, websocket, [undefined | _]}) ->
	false;
precondition(_State, {call, ?MODULE, http, [notpartier, put, undefined, _Json, _Name]}) ->
	false;
precondition(_State, {call, ?MODULE, http, [_Who, post, undefined, _Json, _Name]}) ->
	true;
precondition(_State, {call, ?MODULE, http, [_Who, post, _Whatever, _Json, _Name]}) ->
	false;
precondition(_State, _Call) ->
	true.

%% === next_state ==========================================

next_state(State, _Result, {call, ?MODULE, http, [_Who, get | _]}) ->
	State;

next_state(State, _Result, {call, ?MODULE, http, [notauthed | _]}) ->
	State;

next_state(State, Result, {call, ?MODULE, http, [Who, post, undefined, Json, Name]}) ->
	?debugFmt("~n"
		"=====================~n"
		"== in case of fail ==~n"
		"=====================~n"
		"    State: ~p~n"
		"    Result: ~p~n"
		"    Who: ~p~n"
		"    Json: ~p~n"
		"    Name: ~p~n", [State, Result, Who, Json, Name]),
	Json2 = maybe_set_name(Json, Name),
	PutValid = map_put_valid(Json2, undefined),
	NameUnique = map_name_unique(Name, Who, undefined, State#state.maps),
	if
		PutValid andalso NameUnique ->
			TestMap = extract_map(undefined, Who, Result, Name),
			TestMap2 = TestMap#test_map{name = Name},
			TestLayer = rpgb_layers_tests:extract_layer(undefined, Who, {map, Result}),
			TestLayer1 = TestLayer#test_layer{map_id = TestMap2#test_map.id},
			Maps = State#state.maps ++ [TestMap2],
			Layers = State#state.layers ++ [TestLayer1],
			State#state{maps = Maps, layers = Layers};
		true ->
			State
	end;

next_state(State, _Result, {call, ?MODULE, http, [_Who, put, undefined, _Json, _Name]}) ->
	State;

next_state(State, Result, {call, ?MODULE, http, [Who, put, #test_map{owner = Who} = Original, Json, Name]}) ->
	Json2 = maybe_set_name(Json, Name),
	PutValid = map_put_valid(Json2, Original),
	NameUnique = map_name_unique(Who, Name, Original, State#state.maps),
	if
		PutValid andalso NameUnique ->
			NewMap = extract_map(Original, Who, Result, Name),

			Maps = lists:keyreplace(Original#test_map.id, #test_map.id, State#state.maps, NewMap),
			State#state{maps = Maps};
		true ->
			State
	end;

next_state(State, _Result, {call, ?MODULE, http, [_Who, put, _Map, _Json, _Name]}) ->
	State;

next_state(State, _Result, {call, ?MODULE, http, [_Who, delete, undefined, _Json, _Name]}) ->
	State;

next_state(State, _Result, {call, ?MODULE, http, [Who, delete, #test_map{owner = Who} = Map, _Json, _Name]}) ->
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

next_state(State, _Result, {call, ?MODULE, http, [_Who, delete, _Map, _Json, _Name]}) ->
	State;

next_state(State, Result, {call, ?MODULE, websocket, [{Socket, Who, MapId}, put, Json, Name]}) ->
	Map = lists:keyfind(MapId, #test_map.id, State#state.maps),
	case Map of
		#test_map{owner = Who} ->
			PutValid = map_put_valid(maybe_set_name(Json, Name), Map),
			NameUnique = map_name_unique(Who, Name, Map, State#state.maps),
			if
				PutValid andalso NameUnique ->
					Map2 = extract_map(Map, Who, Result, Name),
					Maps = lists:keyreplace(MapId, #test_map.id, State#state.maps, Map2),
					State#state{maps = Maps};
				true ->
					State
			end;
		false ->
			Ws = lists:keydelete(Socket, 1, State#state.ws),
			State#state{ws = Ws};
		_NotWho ->
			State
	end;

next_state(State, _Result, {call, ?MODULE, websocket, _Args}) ->
	?debugFmt("next_state for websocket", []),
	State.

map_put_valid(Json, undefined) ->
	Name = proplists:get_value(<<"name">>, rpgb_requests_tests:fix_json_keys(Json)),
	case Name of
		undefined ->
			false;
		<<>> ->
			false;
		null ->
			false;
		_ ->
			true
	end;
map_put_valid(Json, _Map) ->
	Name = proplists:get_value(<<"name">>, rpgb_requests_tests:fix_json_keys(Json)),
	case Name of
		undefined -> true;
		<<>> -> false;
		null -> false;
		Binary when is_binary(Binary) -> true;
		_ -> false
	end.

map_name_unique(Name, Who, Map, Maps) ->
	MapId = case Map of
		#test_map{id = Id} -> Id;
		undefined -> undefined
	end,
	Filtered = [M || #test_map{owner = Lwho, id = Lid, name = Lname} = M <- Maps,
		Lid =/= MapId, Lname =:= Name, Lwho =:= Who],
	case Filtered of
		[] ->
			true;
		_ ->
			false
	end.

extract_map(Original, Who, Result, Name) ->
	M1 = case Original of
		undefined ->
			#test_map{id = {call, rpgb_requests_tests, extract_json, [Result, <<"id">>]},
				url = {call, rpgb_requests_tests, extract_json, [Result, <<"url">>]},
				name = Name,
				ws_url = {call, rpgb_requests_tests, extract_json, [Result, <<"websocketUrl">>]},
				owner = Who};
			_ when is_atom(Name); Name =:= <<>> ->
				Original;
			_ ->
				Original#test_map{name = Name}
	end,
	Name2 = case Name of
		_ when is_atom(Name) ->
			M1#test_map.name;
		<<>> ->
			M1#test_map.name;
		_ when is_binary(Name) ->
			Name;
		_ ->
			M1#test_map.name
	end,
	M1#test_map{
		name = Name2,
		properties = {call, rpgb_requests_tests, extract_json, [Result]}
	}.

%% === postcondition =======================================

postcondition(_State, {call, ?MODULE, http, [_Who, _Action, _Map, _Json, _Name]}, {ok, "404", _Heads, _Body}) ->
	?debugMsg("404"),
	false;

postcondition(_State, {call, ?MODULE, http, [_Who, delete, undefined, _Json, _Name]}, {ok, "405", _Heads, _Body}) ->
	?debugMsg("405 on delete maps"),
	true;

postcondition(_State, {call, ?MODULE, http, [_Who, put, undefined, _Json, _Name]}, {ok, "405", _Heads, _Body}) ->
	?debugMsg("405 on put on maps"),
	true;

postcondition(_State, {call, ?MODULE, http, [notauthed, _Action, _Map, _Json, _Name]}, {ok, "401", Heads, _Body}) ->
	?debugMsg("401"),
	proplists:get_value("www-authenticate", Heads) =:= "persona";

postcondition(_State, {call, ?MODULE, http, [Who, _Action, #test_map{owner = Who}, _Json, _Name]}, {ok, "405", _Heads, _Body}) ->
	?debugMsg("405 on some action on maps/mapid"),
	false; 

postcondition(State, {call, ?MODULE, http, [Who, post, undefined, _Json, Name]}, {ok, "409", _Headers, "\"You already have a map by that name.\""}) ->
	not map_name_unique(Who, Name, undefined, State#state.maps);

postcondition(_State, {call, ?MODULE, http, [_Who, post, undefined, _Json, Name]}, {ok, "422", _Headers, _Body}) ->
	case Name of
		undefined ->
			true;
		<<>> ->
			true;
		null ->
			true;
		_ ->
			false
	end;

postcondition(_State, {call, ?MODULE, http, [_Who, put, _Map, _Json, Name]}, {ok,"422", _Heads, _Body}) ->
	case Name of
		undefined -> false;
		null -> true;
		<<>> -> true;
		_ -> false
	end;

postcondition(_State, {call, ?MODULE, http, [_Who, post, undefined, Json, Name]}, {ok, "201", Headers, Body}) ->
	?debugMsg("201 on post to maps/"),
	HasLocation = proplists:get_value("location", Headers) =/= undefined,
	ExpectedBody = jsx:to_term(jsx:to_json(maybe_set_name(Json, Name))),
	DecodedBody = jsx:to_term(list_to_binary(Body)),
	rpgb_requests_tests:assert_json(ExpectedBody, DecodedBody) andalso HasLocation;

postcondition(_State, {call, ?MODULE, http, [notpartier, _Action, _Map, _Json, _Name]}, {ok, "403", _Heads, _Body}) ->
	?debugMsg("403"),
	true;

postcondition(State, {call, ?MODULE, http, [Who, put, #test_map{owner = Who, id = MapId}, Json, Name]}, {ok, "200", _Headers, Body}) ->
	?debugMsg("200 to pust on maps/mapid"),
	PostedBody = jsx:to_term(jsx:to_json(maybe_set_name(Json, Name))),
	DecodedBody = jsx:to_term(list_to_binary(Body)),
	JsonOkay = rpgb_requests_tests:assert_json(PostedBody, DecodedBody),
	JsonOkay andalso lists:all(fun
		({Socket, SomeWho, Mid}) when Mid =:= MapId ->
			?debugFmt("asserting ~s's websokcet got update for map ~p", [SomeWho, Mid]),
			rpgb_requests_tests:assert_ws_frame(Socket, put, undefined, map, MapId, PostedBody);
		(_) ->
			true
	end, State#state.ws);

postcondition(State, {call, ?MODULE, http, [Who, get, undefined, _Json, _Name]}, {ok, "200", _Headers, Body}) ->
	?debugMsg("200 on get to maps"),
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

postcondition(_State, {call, ?MODULE, http, [_Who, put, _Map, _Json, _Name]}, {ok, "403", _Headers, _Body}) ->
	?debugMsg("403 on put"),
	true;

postcondition(_State, {call, ?MODULE, http, [_Who, get, Map, _Json, _Name]}, {ok, "200", _Headers, Body}) ->
	?debugFmt("200 on get maps/mapid: ~p", [Body]),
	GotBody = jsx:to_term(list_to_binary(Body)),
	rpgb_requests_tests:assert_json(Map#test_map.properties, GotBody);

postcondition(State, {call, ?MODULE, http, [Who, delete, #test_map{owner = Who, id = MapId}, _Json, _Name]}, {ok, "204", _Headers, _Body}) ->
	?debugMsg("204 on delete to maps/mapid"),
	lists:all(fun
		({S, _SomeWho, Mid}) when MapId =:= Mid ->
			rpgb_requests_tests:assert_ws_frame(S, delete, undefined, map, MapId, undefined);
		(_) ->
			true
	end, State#state.ws);

postcondition(_State, {call, ?MODULE, http, [_Who, delete, _Map, _Json, _Name]}, {ok, "403", _Headers, _Body}) ->
	?debugMsg("403 on delete to maps"),
	true;

postcondition(_State, {call, ?MODULE, websocket, _Args}, {_Reply, {error, Wut}}) ->
	?debugFmt("error on first reply for websocket: ~p", [Wut]),
	false;

postcondition(State, {call, ?MODULE, websocket, [{Socket, _Who, MapId}, get, _Json, _Name]}, {ReplyTo, ReplyFrame}) ->
	?debugMsg("websocket get maps"),
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			ErrorFrame = rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, undefined),
			Next = gen_websocket:recv(Socket, 1000),
			ErrorFrame andalso Next == {error, timeout};
		Map ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ReplyTo, Map#test_map.properties)
	end;

postcondition(_State, {call, ?MODULE, websocket, [_Socket, delete, _Json, _Name]}, {ReplyTo, ReplyFrame}) ->
	?debugMsg("websocket delete"),
	rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, undefined);

%postcondition(State, {call, ?MODULE, websocket, [{Socket, _Who, MapId}, Action, Json]}, {ReplyTo, ReplyFrame}) ->
%	ReplyFrameOkay = rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ReplyTo, Json),
%	OtherSockets = [Other || {_S, _W, Mid} = Other <- State#state.ws, Mid =:= MapId],
%	Others = lists:all(fun({S,_W,_M}) ->
%		rpgb_requests_tests:assert_ws_frame(S, put, undefined, maps, MapId, Json)
%	end, OtherSockets),
%	ReplyFrameOkay andalso Others;

postcondition(State, {call, ?MODULE, websocket, [{Socket, Who, MapId}, put, Json, Name]}, {ReplyTo, #ws_msg{action = <<"reply">>, accepted = false, type_id = ReplyTo, type = <<"reply">>} = ReplyFrame}) ->
	Map = lists:keyfind(MapId, #test_map.id, State#state.maps),
	case Map of
		false ->
			?debugFmt("Could not find map ~p", [MapId]),
			{error, closed} =:= gen_websocket:recv(Socket, 1000);
		#test_map{owner = Who} ->
			Json2 = maybe_set_name(Json, Name),
			NameUnique = map_name_unique(Who, Name, Map, State#state.maps),
			NameValid = map_put_valid(Json2, Map),
			if
				NameUnique andalso NameValid ->
					false;
				NameUnique ->
					<<"Name cannot be blank.">> =:= ReplyFrame#ws_msg.data;
				NameValid ->
					<<"You already have a map by that name.">> =:= ReplyFrame#ws_msg.data;
				true ->
					false
			end;
		_NotWho ->
			<<"not owner">> =:= ReplyFrame#ws_msg.data
	end;

postcondition(State, {call, ?MODULE, websocket, [{Socket, Who, MapId}, put, Json, Name]}, {ReplyTo, ReplyFrame}) ->
	?debugFmt("The reply to is ~p", [ReplyTo]),
	Map = lists:keyfind(MapId, #test_map.id, State#state.maps),
	case Map of
		false ->
			?debugFmt("Could not find map ~p", [MapId]),
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, <<"not owner">>),
			Closed = gen_websocket:recv(Socket, 1000),
			Closed == {error, closed};
		#test_map{owner = Who} ->
			NameUnique = map_name_unique(Who, Name, Map, State#state.maps),
			Json2 = maybe_set_name(Json, Name),
			NameValid = map_put_valid(Json2, Map),
			if
				NameUnique andalso NameValid ->
					FirstFrame = rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ReplyTo, Json2),
					AllOthers = lists:all(fun
						({S, _W, Mid} = Sdata) when Mid =:= MapId ->
							?debugFmt("dealing with socket ~p", [Sdata]),
							rpgb_requests_tests:assert_ws_frame(S, put, undefined, map, MapId, maybe_set_name(Json, Name));
						(_) ->
							true
					end, State#state.ws),
					FirstFrame andalso AllOthers;
				NameUnique ->
					rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, <<"Name cannot be blank.">>);
				true ->
					rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, <<"You already have a map by that name.">>)
			end;
		_NotWho ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, <<"not owner">>)
	end;

postcondition(_State, {call, ?MODULE, websocket, [{_Socket, _Who, _MapId}, post, _Json, _Name]}, {ReplyTo, ReplyFrame}) ->
	?debugMsg("websocket post"),
	rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ReplyTo, <<"invalid method">>);

postcondition(State, Call, Res) ->
	?debugFmt("Postcondition fallthrough~n"
		"    State: ~p~n"
		"    Call: ~p~n"
		"    Res: ~p", [State, Call, Res]),
		false.
