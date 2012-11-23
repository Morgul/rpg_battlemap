-module(rpgb_handle_layer_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-define(map_url, "http://localhost:9097/map/9000/layers").
-define(map_url(LayerId), ?map_url ++ "/" ++ integer_to_list(LayerId)).
-define(layer_url, "http://localhost:9097/layer").
-define(layer_url(LayerId), ?layer_url ++ "/" ++ integer_to_list(LayerId)).
-define(mapid, 9000).
-define(cookie, begin
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"sessionid">>),
	{"Cookie", binary_to_list(Cookie)}
end).
-define(badcookie, begin
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"baduser">>),
	{"Cookie", binary_to_list(Cookie)}
end).
-define(accepts, {"Accept", "application/json"}).
-define(contenttype, {"Content-Type", "application/json"}).

-compile(export_all).

browser_test_() -> {setup, fun() ->
		rpgb_test_util:web_test_setup(?MODULE),
		rpgb_test_util:create_authed_session(<<"sessionid">>),
		rpgb_test_util:create_authed_session(<<"baduser">>)
	end,
	fun(_) ->
		rpgb_test_util:web_test_teardown()
	end,
	fun(_) -> [

		{"statem", timeout, 60000, fun() ->
			?assert(proper:quickcheck(?MODULE:prop_map_statem()))
		end}

	] end}.

prop_map_statem() ->
	?FORALL(Cmds, commands(?MODULE), begin
		{Hist, State, Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(?debugFmt("\n==========================\n== proper check failed! ==\n==========================\n== Hstory ==\n~p\n\n== State ==\n~p\n\n== Result ==\n~p\n", [Hist, State, Res]),
			Res == ok)
	end).

%% =======================================================
%% generators
%% =======================================================

initial_state() ->
	rpgb_data:reset(),
	{ok, Session} = rpgb_session:get(<<"sessionid">>),
	User = rpgb_session:get_user(Session),
	Map = #rpgb_rec_battlemap{
		id = 9000,
		owner_id = User#rpgb_rec_user.id
	},
	{ok, Map} = rpgb_data:save(Map),
	Layer = #rpgb_rec_layer{name = <<"first layer">>, battlemap_id = 9000},
	{ok, Layer1} = rpgb_data:save(Layer),
	Json = [
		{<<"id">>, Layer1#rpgb_rec_layer.id},
		{<<"name">>, <<"first layer">>},
		{<<"url">>, ?layer_url(Layer1#rpgb_rec_layer.id)},
		{<<"battlemap_id">>, 9000}
	],
	[Json].

command(S) ->
	oneof([
		{call, ?MODULE, create_bad_user, [rpgb_prop:g_name(), g_next(S), g_url()]},
		{call, ?MODULE, create_blank_name, [g_next(S), g_url()]},
		{call, ?MODULE, create_name_conflict, [g_next(S), g_existant(S), g_url()]},
		{call, ?MODULE, create_missing_map_id, [rpgb_prop:g_name(), g_next(S)]},
		{call, ?MODULE, create_bad_map_id, [rpgb_prop:g_name(), g_next(S), choose(9999, 19999), g_url()]},
		{call, ?MODULE, create, [rpgb_prop:g_name(), g_next(S), g_url()]},
		{call, ?MODULE, get_layers, []},
		{call, ?MODULE, get_a_layer, [g_url(), g_existant(S)]},
		{call, ?MODULE, update_bad_user, [rpgb_prop:g_name(), g_next(S), g_url(), g_existant(S)]},
		{call, ?MODULE, update_blank_name, [g_next(S), g_url(), g_existant(S)]},
		{call, ?MODULE, update, [oneof([undefined, rpgb_prop:g_name()]), g_next(S), g_existant(S), g_url()]},
		{call, ?MODULE, update_bad_reorder, [choose(90000, 100000), g_existant(S), g_url()]},
		{call, ?MODULE, delete_bad_user, [g_url(), g_existant(S)]},
		{call, ?MODULE, delete_last_layer, [g_url(), g_existant(S)]},
		{call, ?MODULE, delete, [g_url(), g_existant(S)]}
	]).

g_url() ->
	oneof([direct, map]).

g_existant([]) ->
	undefined;
g_existant([Layer]) ->
	Layer;
g_existant(Layers) ->
	Max = length(Layers),
	Nth = crypto:rand_uniform(1, Max),
	lists:nth(Nth, Layers).

g_next(S) ->
	oneof([undefined, g_existant(S)]).

%% =======================================================
%% preconditions
%% =======================================================

precondition(N, {call, _, delete_last_layer, _}) ->
	if
		length(N) == 1 -> true;
		true -> false
	end;
precondition(N, {call, _, delete, _}) ->
	if
		length(N) > 1 -> true;
		true -> false
	end;
precondition(N,_) ->
	length(N) >= 1.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, {ok, _, _, _} = Res, {call, _, create, _}) ->
	LayerJson = decode_json_body(Res),
	insert_layer(LayerJson, State);

next_state(State, Res, {call, _, create, _}) ->
	{call, ?MODULE, insert_layer, [
		{call, ?MODULE, decode_json_body, [Res]},
	State]};

next_state(State, {ok, _, _, _} = Res, {call, _, delete, [_UrlType, Layer]}) ->
	lists:delete(Layer, State);

next_state(State, Res, {call, _, delete, [_UrlType, Existant]}) ->
	{call, lists, delete, [Existant, State]};

next_state(State, {ok, _, _, _} = Res, {call, _, update, [_UrlType, Existant]}) ->
	LayerJson = decode_json_body(Res),
	List = lists:delete(Existant, State),
	insert_layer(LayerJson, List);

next_state(State, Res, {call, _, update, [_UrlType, Existant]}) ->
	{call, ?MODULE, insert_layer, [{call, ?MODULE, decode_json_body, [Res]}, {call, lists, delete, [Existant, State]}]};
next_state(State, _Res, _Call) ->
	State.

decode_json_body({ok, _, _, Body}) ->
	jsx:to_term(list_to_binary(Body)).

insert_layer(Layer, LayerList) ->
	case proplists:get_value(<<"next_layer_id">>, Layer) of
		undefined ->
			LayerList ++ [Layer];
		NextId ->
			insert_layer(NextId, Layer, LayerList, [])
	end.

insert_layer(_Needle, _Layer, [], _Backup) ->
	erlang:error(badarg);
insert_layer(Needle, Layer, [MaybeNext | Tail] = LayerList, Backup) ->
	case proplists:get_value(<<"id">>, MaybeNext) of
		Needle ->
			unwind([Layer | LayerList], Backup);
		_NotNeedle ->
			insert_layer(Needle, Layer, Tail, [MaybeNext | Backup])
	end.

unwind(Nacc, []) ->
	Nacc;
unwind(Nacc, [H | T]) ->
	unwind([H | Nacc], T).

%% =======================================================
%% tests proper
%% =======================================================

create_bad_user(Name, Next, UrlType) ->
	Json = make_json(Name, Next, UrlType),
	Url = case_url_type(UrlType, []),
	ibrowse:send_req(Url, [?badcookie, ?accepts, ?contenttype], put, Json).

create_blank_name(Next, UrlType) ->
	Json = make_json(<<>>, Next, UrlType),
	Url = case_url_type(UrlType, []),
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, Json).

create_name_conflict(Next, Existant, UrlType) ->
	Name = proplists:get_value(<<"name">>, Existant),
	Json = make_json(Name, Next, UrlType),
	Url = case_url_type(UrlType, []),
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, Json).

create_missing_map_id(Name, Next) ->
	Json = make_json(Name, Next, undefined),
	ibrowse:send_req(?layer_url, [?cookie, ?accepts, ?contenttype], put, Json).

create_bad_map_id(Name, Next, MapId, UrlType) ->
	Json = make_json(Name, Next, MapId),
	Url = case_url_type(UrlType, []),
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, Json).

create(Name, Next, UrlType) ->
	Json = make_json(Name, Next, UrlType),
	Url = case_url_type(UrlType, []),
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, Json).

get_layers() ->
	ibrowse:send_req(?map_url, [?cookie, ?accepts, ?contenttype], get, []).

get_a_layer(UrlType, Layer) ->
	Url = case_url_type(UrlType, Layer),
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], get, []).

update_bad_user(Name, Next, UrlType, Layer) ->
	Url = case_url_type(UrlType, Layer),
	Json = make_json(Name, Next),
	ibrowse:send_req(Url, [?badcookie, ?accepts, ?contenttype], put, Json).

update_blank_name(Next, UrlType, Layer) ->
	Json = make_json(<<>>, Next),
	Url = case_url_type(UrlType, Layer),
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, Json).

update(Name, Next, Layer, UrlType) ->
	Json = make_json(Name, Next),
	Url = case_url_type(UrlType, Layer),
	ibrowse:send_req(Url, [?contenttype, ?accepts, ?cookie], put, Json).

update_bad_reorder(BadNext, Layer, UrlType) ->
	Json = make_json(undefined, BadNext),
	Url = case_url_type(UrlType, Layer),
	ibrowse:send_req(Url, [?contenttype, ?accepts, ?cookie], put, Json).

delete_bad_user(UrlType, Layer) ->
	Url = case_url_type(UrlType, Layer),
	ibrowse:send_req(Url, [?contenttype, ?badcookie, ?accepts], delete, []).

delete_last_layer(UrlType, Layer) ->
	Url = case_url_type(UrlType, Layer),
	ibrowse:send_req(Url, [?contenttype, ?cookie, ?accepts], delete, []).

delete(UrlType, Layer) ->
	Url = case_url_type(UrlType, Layer),
	ibrowse:send_req(Url, [?contenttype, ?cookie, ?accepts], delete, []).

make_json(Name, Next) ->
	jsx:to_json(make_json_(Name, Next)).

make_json_(undefined, undefined) ->
	[{}];
make_json_(Name, undefined) ->
	[{name, Name}];
make_json_(undefined, Next) ->
	[{next_layer_id, Next}];
make_json_(Name, Next) ->
	[{name, Name}, {next_layer_id, Next}].

make_json(Name, Next, Type) ->
	jsx:to_json(make_json_(Name, Next, Type)).

make_json_(Name, Next, Type) ->
	case {make_json(Name, Next), Type} of
		{[{}], direct} ->
			[{battlemap_id, 9000}];
		{[{}], map} ->
			[{}];
		{Base, direct} ->
			[{battlemap_id, 9000} | Base];
		{Base, map} ->
			Base
	end.

case_url_type(Type, Layer) ->
	Id = case proplists:get_value(<<"id">>, Layer) of
		undefined ->
			[];
		Int ->
			integer_to_list(Int)
	end,
	case Type of
		direct ->
			?layer_url ++ "/" ++ Id;
		map ->
			?map_url ++ "/" ++ Id
	end.

%% =======================================================
%% postcondition
%% =======================================================

postcondition(_Layers, {call, _, create, [_UrlType, Name, NextId]}, {ok, "201", Heads, Body}) ->
	Location = proplists:get_value("Location", Heads),
	?assertNotEqual(undefined, Location),
	BodyJson = jsx:to_term(list_to_binary(Body)),
	?assertEqual(list_to_binary(Location), proplists:get_value(<<"url">>, BodyJson)),
	?assertEqual(Name, proplists:get_value(<<"name">>, BodyJson)),
	?assertEqual(NextId, proplists:get_value(<<"next_layer_id">>, BodyJson)),
	true;

postcondition(_Layers, {call, _, create_bad_user, _}, {ok, "400", _, _}) ->
	true;

postcondition(_Layers, {call, _, create_name_conflict, _}, {ok, "409", _, _}) ->
	true;

postcondition(_Layers, {call, _, create_missing_map_id, _}, {ok, "412", _, _}) ->
	true;

postcondition(_Layers, {call, _, create_bad_map_id, _}, {ok, "400", _, _}) ->
	true;

postcondition(Layers, {call, _, get_layers, _}, {ok, "200", _, Body}) ->
	BodyJson = jsx:to_term(list_to_binary(Body)),
	assert_layer_list(Layers, BodyJson);

postcondition(Layers, {call, _, get_a_layer, [_UrlType, Existant]}, {ok, "200", _, Body}) ->
	BodyJson = jsx:to_term(list_to_binary(Body)),
	assert_layer(Existant, BodyJson);

postcondition(_Layers, {call, _, update_bad_user, _}, {ok, "400", _, _}) ->
	true;

postcondition(_Layers, {call, _, update_blank_name, _}, {ok, "412", _, _}) ->
	true;

postcondition(_Layers, {call, _, update, [Name, NextId, Existant, _UrlType]}, {ok, "200", _, Body}) ->
	BodyJson = jsx:to_term(list_to_binary(Body)),
	Expected = update_existant([{<<"name">>, Name}, {<<"next_layer_id">>, NextId}], Existant),
	assert_layer(Expected, BodyJson);

postcondition(_Layers, {call, _, update_bad_reorder, _}, {ok, "412", _, _}) ->
	true;

postcondition(_Layers, {call, _, delete_bad_user, _}, {ok, "400", _, _}) ->
	true;

postcondition(_Layers, {call, _, delete_last_layer, _}, {ok, "412", _, _}) ->
	true;

postcondition(_Layers, {call, _, delete, _}, {ok, "204", _, _}) ->
	true;

postcondition(_Layers, _Call, _Res) ->
	false.

assert_layer_list(Expected, Got) ->
	?assertEqual(length(Expected), length(Got)),
	assert_layer_list_(Expected, Got).

assert_layer_list_([EH | ET], [GH | GT]) ->
	assert_layer(EH, GH),
	assert_layer_list_(ET, GT).

assert_layer(Expected, Got) ->
	?assertEqual(length(Expected), length(Got)),
	OrderedE = orddict:from_list(Expected),
	OrderedG = orddict:from_list(Got),
	?assertEqual(OrderedE, OrderedG).

update_existant([], Acc) ->
	Acc;

update_existant([{K, V} | T], Acc) ->
	Acc2 = proplists:delete(K, Acc),
	update_existant(T, [{K, V} | Acc2]).