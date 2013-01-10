-module(rpgb_handle_zone_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-define(zone_aura_url(Mode), "http://localhost:9098/map/" ++ integer_to_list(?mapid) ++ "/layers/" ++ integer_to_list(?layerid) ++ "/" ++ Mode).
-define(zone_url, ?zone_aura_url("zones")).
-define(zone_url(ZoneId), if is_integer(ZoneId) -> ?zone_url ++ "/" ++ integer_to_list(ZoneId); true -> binary_to_list(proplists:get_value(<<"url">>, ZoneId)) end).
-define(aura_url, ?zone_aura_url("auras")).
-define(aura_url(AuraId), if is_integer(AuraId) -> ?aura_url ++ "/" ++ integer_to_list(AuraId); true -> binary_to_list(proplists:get_value(<<"url">>, AuraId)) end).
-define(mapid, 9000).
-define(layerid, 3000).
-define(accepts, {"Accept", "application/json"}).
-define(contenttype, {"Content-Type", "application/json"}).
-define(req_headers, [?accepts, ?contenttype]).
-define(req_headers(OtherHeads), OtherHeads ++ ?req_headers).

-compile(export_all).

-record(state, {
	zones = [],
	auras = []
}).

browser_test_() -> {setup, fun() ->
		rpgb_test_util:web_test_setup(?MODULE)
	end,
	fun(_) ->
		rpgb_test_util:web_test_teardown()
	end,
	fun(_) -> [

		{"statem", timeout, 60000, fun() ->
			?assert(proper:quickcheck(?MODULE:prop_zone_statem()))
		end}
	] end}.

prop_zone_statem() ->
	?FORALL(Cmds, commands(?MODULE), begin
		{Hist, State, Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(?debugFmt("proper check failed!\n== History ==\n~p\n\n== State ==\n~p\n\n== Result ==\n~p", [Hist, State, Res]),
		Res == ok)
	end).

%% =======================================================
%% generators
%% =======================================================

initial_state() ->
	rpgb_data:reset(),
	rpgb_test_util:create_authed_session(<<"owner_session">>, <<"map_owner">>),
	rpgb_test_util:create_authed_session(<<"baduser">>, <<"explodeier">>),
	rpgb_test_util:create_authed_session(<<"partier1_session">>, <<"partier1">>),
	rpgb_test_util:create_authed_session(<<"partier2_session">>, <<"partier2">>),
	{ok, UserSession} = rpgb_session:get(<<"owner_session">>),
	User = rpgb_session:get_user(UserSession),
	{ok, P1s} = rpgb_session:get(<<"partier1_session">>),
	P1 = rpgb_session:get_user(P1s),
	{ok, P2s} = rpgb_session:get(<<"partier2_session">>),
	P2 = rpgb_session:get_user(P2s),
	rpgb_data:save(#rpgb_rec_layer{
		name = <<"primary layer">>,
		id = ?layerid,
		battlemap_id = ?mapid,
		next_layer_id = undefined
	}),
	Map = #rpgb_rec_battlemap{
		id = ?mapid,
		owner_id = User#rpgb_rec_user.id,
		participant_ids = [P1#rpgb_rec_user.id, P2#rpgb_rec_user.id],
		bottom_layer_id = ?layerid
	},
	{ok, Map2} = rpgb_data:save(Map),
	#state{zones = [], auras = []}.

command(S) ->
	oneof([
		{call, ?MODULE, create_zone, [g_zone(), rpgb_prop:g_name(), g_next(zone, S), S]},
		{call, ?MODULE, create_aura, [g_aura(), rpgb_prop:g_name(), g_next(aura, S), oneof(['owner', 'partier1', 'partier2']), S]},
%
%		{call, ?MODULE, create_zone_bad_user, [g_zone(), g_name(), g_layer(), g_next(zone, S), oneof(['partier1', partier2, baduser]), S]},
%		{call, ?MODULE, create_aura_bad_user, [g_aura(), g_name(), g_layer(), g_next(aura, S), S]},
%
		{call, ?MODULE, get_zones, [oneof(['owner', 'partier1', 'partier2'])]},
%		{call, ?MODULE, get_auras, [g_layer(), oneof(['owner', 'partier1', 'partier2'])]},
%		{call, ?MODULE, get_a_zone, [g_zone(S), oneof(['owner', 'partier1', 'partier2']), S]},
%		{call, ?MODULE, get_an_aura, [g_aura(S), oneof(['owner', 'partier1', 'partier2']), S]},
%
%		{call, ?MODULE, get_bad_user, [g_existant(S), S]},
%
%		{call, ?MODULE, update_zone, [g_zone(), g_maybe_layer(), g_maybe_next(zone, S), S]},
%		{call, ?MODULE, update_aura, [g_aura(), g_maybe_layer(), g_maybe_next(zone, S), S]},
%
%		{call, ?MODULE, update_zone_bad_user, [g_zone(), g_maybe_layer(), g_maybe_next(zone, S), oneof(['baduser', 'partier1', 'partier2']), S]},
%		{call, ?MODULE, update_aura_bad_user, [g_zone(), g_maybe_layer(), g_maybe_next(aura, S), oneof(['baduser', 'wrong_partier']), S]},
%
		{call, ?MODULE, delete_zone, [g_existant(zone, S), S]},
		{call, ?MODULE, delete_aura, [g_existant(aura, S), oneof(['owner', 'partier1', 'partier2']), S]}
	]).

g_next(zone, #state{zones = []}) ->
	null;
g_next(zone, #state{zones = Zones}) ->
	oneof([undefined, null, g_existant(Zones)]);
g_next(aura, #state{auras = []}) ->
	null;
g_next(aura, #state{auras = Auras}) ->
	oneof([undefined, null, g_existant(Auras)]).

g_existant([]) ->
	undefined;
g_existant([_Elem]) ->
	1;
g_existant(List) ->
	Max = length(List),
	choose(1, Max).

g_existant(zone, #state{zones = Zones}) ->
	g_existant(Zones);
g_existant(aura, #state{auras = Auras}) ->
	g_existant(Auras).

g_zone() ->
	g_zone_or_aura(<<"zone">>).

g_aura() ->
	g_zone_or_aura(<<"aura">>).

g_zone_or_aura(_Type) ->
	g_zone_aura_field().

g_zone_aura_field() ->
	?LET({Basic, Element}, begin
		BasicFields = list(oneof([
			{<<"name">>, rpgb_prop:g_name()},
			{<<"rotation">>, float(-360.0, 360.0)},
			{<<"stroke_color">>, rpgb_prop:g_color()},
			{<<"stroke_width">>, choose(0, 10)},
			{<<"stroke_opacity">>, float(0.0, 1.0)},
			{<<"fill_color">>, rpgb_prop:g_color()}
		])),
		ElementFields = g_element_fields(),
		{BasicFields, ElementFields}
	end, begin
		rpgb_prop:uniquify(Basic ++ Element)
	end).

g_element_fields() ->
	?LET(Element, oneof(['rect', 'circle', 'ellipse', 'line', 'polyline', 'polygon', 'path']), g_make_element_attrs(Element)).

g_make_element_attrs(rect) ->
	[{<<"element_type">>, <<"rect">>},
	{<<"element_attrs">>, [
		{<<"x">>, choose(-100, 100)},
		{<<"y">>, choose(-100, 100)},
		{<<"width">>, choose(1, 100)},
		{<<"height">>, choose(1, 100)}
	]}];
g_make_element_attrs(circle) ->
	[{<<"element_type">>, <<"circle">>},
	{<<"element_attrs">>, [
		{<<"cx">>, choose(-100, 100)},
		{<<"cy">>, choose(-100, 100)},
		{<<"r">>, choose(1, 100)}
	]}];
g_make_element_attrs(ellipse) ->
	[{<<"element_type">>, <<"ellipse">>},
	{<<"element_attrs">>, [
		{<<"cx">>, choose(-100, 100)},
		{<<"cy">>, choose(-100, 100)},
		{<<"rx">>, choose(1, 100)},
		{<<"ry">>, choose(1, 100)}
	]}];
g_make_element_attrs(line) ->
	[{<<"element_type">>, <<"line">>},
	{<<"element_attrs">>, [
		{<<"x1">>, choose(-100, 100)},
		{<<"y1">>, choose(-100, 100)},
		{<<"x2">>, choose(-100, 100)},
		{<<"y2">>, choose(-100, 100)}
	]}];
g_make_element_attrs(polyline) ->
	[{<<"element_type">>, <<"polyline">>},
	{<<"element_attrs">>, [
		{<<"points">>, g_point_list()}
	]}];
g_make_element_attrs(polygon) ->
	[{<<"element_type">>, <<"polygon">>},
	{<<"element_attrs">>, [
		{<<"points">>, g_point_list()}
	]}];
g_make_element_attrs(path) ->
	[{<<"element_type">>, <<"path">>},
	{<<"element_attrs">>, [
		{<<"d">>, g_path()}
	]}].

g_point_list() ->
	?LET({Point1, Points},
		{g_point(), list(g_point())},
		begin
			list_to_binary([integer_to_list(X) ++ [$,] ++ integer_to_list(Y) ++ " " || {X,Y} <- [Point1 | Points]])
		end).

g_path() ->
	?LET(PathSegments, list(g_path_segment()),
	list_to_binary(string:join(PathSegments, " "))).

g_path_segment() ->
	?LET(Type, oneof("MmZzLlHhVvCcSsQqTtAa"), g_path_segment_data(Type)).

g_path_segment_data(ML) when ML =:= $m; ML =:= $M; ML =:= $l; ML =:= $L; ML =:= $t; ML =:= $T ->
	?LET(Points,
		list(g_point()),
		begin
			lists:flatten([ML, $ ] ++ [integer_to_list(X) ++ " " ++ integer_to_list(Y) || {X,Y} <- Points])
		end);
g_path_segment_data(Z) when Z =:= $z; Z =:= $Z ->
	[Z];
g_path_segment_data(HV) when HV =:= $h; HV =:= $H; HV =:= $v; HV =:= $V ->
	?LET(Moves, list(g_xy()),
		begin
			string:join([[HV]] ++ [integer_to_list(M) || M <- Moves], " ")
		end);
g_path_segment_data(C) when C =:= $c; C =:= $C ->
	?LET(Curves, list({g_point(), g_point(), g_point()}),
		begin
			Curves2 = lists:flatten([[A,B,C,D,E,F] || {{A,B},{C,D},{E,F}} <- Curves]),
			string:join([[C]] ++ [integer_to_list(C) || C <- Curves2], " ")
		end);
g_path_segment_data(S) when S =:= $s; S =:= $S; S =:= $q; S =:= $Q ->
	?LET(Curves, list({g_point(), g_point()}),
		begin
			Curves2 = lists:flatten([[A,B,C,D] || {{A,B},{C,D}} <- Curves]),
			string:join([[S]] ++ [integer_to_list(C) || C <- Curves2], " ")
		end);
g_path_segment_data(A) when A =:= $a; A =:= $A ->
	?LET(Params, {g_xy(), g_xy(), choose(-360, 360), choose(0,1), choose(0,1), g_xy(), g_xy()},
		begin
			List = tuple_to_list(Params),
			List2 = [integer_to_list(N) || N <- List],
			string:join([[A]] ++ List2, " ")
		end).

g_xy() ->
	choose(-100, 100).

g_point() ->
	{g_xy(), g_xy()}.

%% =======================================================
%% preconditions
%% =======================================================

precondition(S, {call, _, create_zone, _}) ->
	true;
precondition(S, {call, _, create_aura, _}) ->
	true;
precondition(S, {call, _, delete_zone, [_, #state{zones = Z}]}) when length(Z) >= 1 ->
	true;
precondition(S, {call, _, delete_aura, [_, _, #state{auras = A}]}) when length(A) >= 1 ->
	true;
precondition(S, {call, _, get_zones, _}) ->
	true;
precondition(_,_) ->
	false.

%% =======================================================
%% next_state
%% =======================================================

next_state(#state{zones = Zones} = State, Res, {call, _, create_zone, [_Put, _Name, Next, _S]}) ->
	Zones2 = if
		is_atom(Next) ->
			Zones ++ [{call, ?MODULE, decode_res, [Res]}];
		true ->
			rpgb:splice(Zones, Next, 0, [{call, ?MODULE, decode_res, [Res]}])
	end,
	State#state{zones = Zones2};

next_state(#state{auras = Auras} = State, Res, {call, _, create_aura, [_Put, _Name, Next, _Putter, _S]}) ->
	Auras2 = if
		is_atom(Next) ->
			Auras ++ [{call, ?MODULE, decode_res, [Res]}];
		true ->
			rpgb:splice(Auras, Next, 0, [{call, ?MODULE, decode_res, [Res]}])
	end,
	State#state{auras = Auras2};

next_state(#state{zones = Zones} = State, Res, {call, _, delete_zone, [Nth, _]}) ->
	State#state{zones = rpgb:snip(Nth, Zones)};

next_state(#state{auras = Auras} = State, Res, {call, _, delete_aura, [Nth, _, _]}) ->
	State#state{auras = rpgb:snip(Nth, Auras)};

next_state(State, _Res, _Call) ->
	State.

%% =======================================================
%% tests proper
%% =======================================================

create_zone(Zone, Name, Next, State) ->
	#state{zones = Zones} = State,
	NextZoneId = get_next(Next, Zones),
	Json = [{<<"next_zone_id">>, NextZoneId} | Zone],
	Json2 = [{<<"name">>, Name} | proplists:delete(<<"name">>, Json)],
	Json3 = purge_undef(Json2),
	ibrowse:send_req(?zone_url, [owner_cookie(), ?accepts, ?contenttype], put, jsx:to_json(Json3)).

create_aura(Aura, Name, Next, Who, State) ->
	#state{auras = Auras} = State,
	NextZoneId = get_next(Next, Auras),
	Json = [{<<"next_zone_id">>, NextZoneId} | Aura],
	Json2 = [{<<"name">>, Name} | proplists:delete(<<"name">>, Json)],
	Json3 = purge_undef(Json2),
	Cookie = cookie(Who),
	ibrowse:send_req(?aura_url, [Cookie, ?accepts, ?contenttype], put, jsx:to_json(Json3)).

get_zones(Who) ->
	Cookie = cookie(Who),
	ibrowse:send_req(?zone_url, [Cookie, ?accepts, ?contenttype], get, []).

delete_zone(Nth, State) ->
	#state{zones = Zones} = State,
	Zone = lists:nth(Nth, Zones),
	Url = proplists:get_value(<<"url">>, Zone),
	ibrowse:send_req(binary_to_list(Url), [owner_cookie(), ?accepts, ?contenttype], delete, []).

delete_aura(Nth, Deleter, State) ->
	#state{auras = Auras} = State,
	Aura = lists:nth(Nth, Auras),
	Url = proplists:get_value(<<"url">>, Aura),
	Cookie = cookie(Deleter),
	ibrowse:send_req(binary_to_list(Url), [Cookie, ?accepts, ?contenttype], delete, []).
%% =======================================================
%% postcondition
%% =======================================================

postcondition(State, {call, _, create_zone, [Put, Name, Next, _]}, {ok, "201", _, Body}) ->
	#state{zones = Zones} = State,
	NextZoneId = get_next(Next, Zones),
	Json = [{<<"next_zone_id">>, NextZoneId} | Put],
	Json2 = [{<<"name">>, Name} | proplists:delete(<<"name">>, Json)],
	Json3 = purge_undef(Json2),
	?assert(rpgb_test_util:assert_body(Json3, Body)),
	true;

postcondition(State, {call, _, create_aura, [Put, Name, Next, Who, _]}, {ok, "201", _, Body}) ->
	#state{auras = Auras} = State,
	NextZoneId = get_next(Next, Auras),
	Json = [{<<"next_zone_id">>, NextZoneId} | Put],
	Json2 = [{<<"name">>, Name} | proplists:delete(<<"name">>, Json)],
	Json3 = purge_undef(Json2),
	?assert(rpgb_test_util:assert_body(Json3, Body)),
	true;

postcondition(State, {call, _, get_zones, [_Who]}, {ok, "200", _, Body}) ->
	#state{zones = Zones} = State,
	Zones2 = fix_next_ids(Zones),
	Terms = jsx:to_term(list_to_binary(Body)),
	?assertEqual(length(Zones2), length(Terms)),
	Pairs = lists:zip(Zones2, Terms),
	[begin
		Bin = jsx:to_json(T),
		?assert(rpgb_test_util:assert_body(Z, Bin))
	end || {Z, T} <- Pairs],
	true;

postcondition(State, {call, _, delete_zone, _}, {ok, "204", _, _}) ->
	true;

postcondition(State, {call, _, delete_aura, _}, {ok, "204", _, _}) ->
	true;

postcondition(S, C, R) ->
	?debugFmt("Catch all post condition:~n~p~n~p~n~p", [S,C,R]),
	false.

%% =======================================================
%% Internal
%% =======================================================

cookie(owner) -> owner_cookie();
cookie(partier1) -> partier1_cookie();
cookie(partier2) -> partier2_cookie();
cookie(baduser) -> bad_cookie().

owner_cookie() ->
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"owner_session">>),
	{"Cookie", binary_to_list(Cookie)}.

partier1_cookie() ->
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"partier1_session">>),
	{"Cookie", binary_to_list(Cookie)}.

partier2_cookie() ->
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"partier2_session">>),
	{"Cookie", binary_to_list(Cookie)}.

bad_cookie() ->
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"baduser">>),
	{"Cookie", binary_to_list(Cookie)}.

get_next(Next, _) when is_atom(Next) ->
	Next;
get_next(Nth, List) ->
	ZoneAura = lists:nth(Nth, List),
	proplists:get_value(<<"id">>, ZoneAura).

purge_undef([{}]) ->
	[{}];
purge_undef(Json) ->
	case [KV || {_, Value} = KV <- Json, Value =/= undefined] of
		[] ->
			[{}];
		Out ->
			Out
	end.

decode_res({ok, _, _, Body}) ->
	jsx:to_term(list_to_binary(Body)).

fix_next_ids([]) ->
	[];

fix_next_ids(List) ->
	fix_next_ids(List, []).

fix_next_ids([Elem], Acc) ->
	Elem2 = [{<<"next_zone_id">>, null} | proplists:delete(<<"next_zone_id">>, Elem)],
	Acc2 = [Elem2 | Acc],
	lists:reverse(Acc2);

fix_next_ids([Elem | Tail], Acc) ->
	[Next | _] = Tail,
	NextId = proplists:get_value(<<"id">>, Next),
	Elem2 = [{<<"next_zone_id">>, NextId} | proplists:delete(<<"next_zone_id">>, Elem)],
	Acc2 = [Elem2 | Acc],
	fix_next_ids(Tail, Acc2).