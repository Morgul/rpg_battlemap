-module(rpgb_handle_layer_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-define(layer_url, "http://localhost:9097/map/9000/layers").
-define(layer_url(LayerId), if is_integer(LayerId) -> ?layer_url ++ "/" ++ integer_to_list(LayerId); true -> binary_to_list(proplists:get_value(<<"url">>, LayerId)) end).
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
		?WHENFAIL(begin
			{ok, Layers} = rpgb_data:search(rpgb_rec_layer, []),
			{ok, Map} = rpgb_data:get_by_id(rpgb_rec_battlemap, 9000),
			InDb = [Map | Layers],
			?debugFmt("\n==========================\n== proper check failed! ==\n==========================\n== Hstory ==\n~p\n\n== State ==\n~p\n\n== Result ==\n~p\n\n== In Database ==\n~p\n", [Hist, State, Res, InDb])
		end, Res == ok)
	end).

%% =======================================================
%% generators
%% =======================================================

initial_state() ->
	rpgb_data:reset(),
	{ok, Session} = rpgb_session:get(<<"sessionid">>),
	User = rpgb_session:get_user(Session),
	Layer = #rpgb_rec_layer{name = <<"first layer">>, battlemap_id = 9000},
	{ok, Layer1} = rpgb_data:save(Layer),
	Map = #rpgb_rec_battlemap{
		id = 9000,
		owner_id = User#rpgb_rec_user.id,
		bottom_layer_id = Layer1#rpgb_rec_layer.id
	},
	{ok, Map} = rpgb_data:save(Map),
	Json = [
		{<<"id">>, Layer1#rpgb_rec_layer.id},
		{<<"name">>, <<"first layer">>},
		{<<"url">>, list_to_binary(?layer_url(Layer1#rpgb_rec_layer.id))},
		{<<"battlemap_id">>, 9000}
	],
	[Json].

command(S) ->
	oneof([
		%{call, ?MODULE, create_bad_user, [rpgb_prop:g_name(), g_next(S), S]},
		%{call, ?MODULE, create_blank_name, [g_next(S), S]},
		%{call, ?MODULE, create_name_conflict, [g_existant(S), g_next(S), S]},
		%{call, ?MODULE, create_bad_map_id, [rpgb_prop:g_name(), g_next(S), choose(9999, 19999), S]},
		{call, ?MODULE, create, [rpgb_prop:g_name(), g_next(S), S]},
		{call, ?MODULE, get_layers, []},
		%{call, ?MODULE, get_a_layer, [g_existant(S), S]},
		%{call, ?MODULE, update_bad_user, [rpgb_prop:g_name(), g_next(S), g_existant(S), S]},
		%{call, ?MODULE, update_blank_name, [g_next(S), g_existant(S), S]},
		{call, ?MODULE, update, [oneof([undefined, rpgb_prop:g_name()]), oneof([undefined, g_next(S)]), g_existant(S), S]},
		%{call, ?MODULE, update_bad_reorder, [oneof([self, choose(90000, 100000)]), g_existant(S), S]},
		%{call, ?MODULE, delete_bad_user, [g_existant(S), S]},
		%{call, ?MODULE, delete_last_layer, [g_existant(S), S]},
		{call, ?MODULE, delete, [g_existant(S), S]}
	]).

g_existant([]) ->
	undefined;
g_existant([Layer]) ->
	1;
g_existant(Layers) ->
	Max = length(Layers),
	choose(1, Max).

g_next([]) ->
	undefined;
g_next(Layers) ->
	Max = length(Layers),
	oneof([undefined, null, choose(1, Max)]).
%g_existant(Layers) when is_tuple(Layers) ->
%	{call, ?MODULE, oneof, [Layers]};
%g_existant(Layers) ->
%	oneof(Layers).
	%Max = length(Layers),
	%Nth = crypto:rand_uniform(1, Max),
	%lists:nth(Nth, Layers).

%g_next(S) ->
%	?LET(X, oneof([null, g_existant(S)]), case X of
%		null -> null;
%		_ ->

%	end).

%% =======================================================
%% preconditions
%% =======================================================

precondition(N, {call, _, delete_last_layer, _}) ->
	%?debugFmt("length of state:  ~p", [length(N)]),
	length(N) == 1;
precondition(N, {call, _, delete, _}) ->
	length(N) > 1;
precondition(_N, {call, _, update, [_Name, Next, Next, _State]}) ->
	false;
%precondition(_N, {call, _, update, [_, 0, _, _]}) ->
%	true;
%precondition(S, {call, _, update, [_Name, Next, Existant, _]}) ->
%	not ( proplists:get_value(<<"id">>, Existant) =:= proplists:get_value(<<"next_layer_id">>, Next) );
precondition(N,_) ->
	length(N) >= 1.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, {ok, _, _, _} = Res, {call, _, create, [_Name, Next, _S]}) ->
	LayerJson = decode_json_body(Res),
	insert_layer(LayerJson, Next, State);

next_state(State, Res, {call, _, create, [_Name, Next, _S]}) ->
	insert_layer({call, ?MODULE, decode_json_body, [Res]}, Next, State);
	%insert_layer(Res, Next, State);

%next_state(State, Res, {call, _, create, _}) ->
%	{call, ?MODULE, insert_layer, [
%		{call, ?MODULE, decode_json_body, [Res]},
%	State]};

next_state(State, _Res, {call, _, delete, [Nth, _State]}) ->
	delete_layer(Nth, State);

%next_state(State, Res, {call, _, delete, [_UrlType, Existant]}) ->
%	{call, lists, delete, [Existant, State]};

next_state(State, Res, {call, _, update, [_Name, null, Updating, _State]}) ->
	{Head, [_Old | Tail]} = lists:split(Updating -1, State),
	Head ++ Tail ++ [{call, ?MODULE, decode_json_body, [Res]}];
next_state(State, Res, {call, _, update, [_Name, undefined, Updating, _State]}) ->
	{Head, [_Old | Tail]} = lists:split(Updating - 1, State),
	Head ++ [{call, ?MODULE, decode_json_body, [Res]}] ++ Tail;
next_state(State, Res, {call, _, update, [_Name, Next, Updating, _State]}) when Next < Updating ->
	{Head, [_Old | Tail]} = lists:split(Updating - 1, State),
	{Head2, Tail2} = lists:split(Next - 1, Head ++ Tail),
	Head2 ++ [{call, ?MODULE, decode_json_body, [Res]}] ++ Tail2;
next_state(State, Res, {call, _, update, [_Name, Next, Updating, _State]}) when Updating < Next ->
	{Head, NextTail} = lists:split(Next - 1, State),
	{Head2, [_Old | Middle]} = lists:split(Updating - 1, Head),
	Head2 ++ Middle ++ [{call, ?MODULE, decode_json_body, [Res]}] ++ NextTail;

%next_state(State, {ok, _, _, _} = Res, {call, _, update, [Name, Next, Updating, _State]}) ->
%	{Head, [_Old | Tail]} = lists:split(Updating - 1, State),
%	State1 = Head ++ [tombstone] ++ Tail,
%	LayerJson = decode_json_body(Res),
%	State2 = case Next of
%		undefined ->
%			Head ++ [LayerJson] ++ Tail;
%		null ->
%			Head ++ Tail ++ [LayerJson];
%		_ ->
%			{H2, T2} = lists:split(Next - 1, State1),
%			H2 ++ [LayerJson] ++ T2
%	end,
%	lists:delete(tombstone, State2);

%next_state(State, Res, {call, _, update, [_UrlType, Existant]}) ->
%	{call, ?MODULE, insert_layer, [{call, ?MODULE, decode_json_body, [Res]}, {call, lists, delete, [Existant, State]}]};
next_state(State, Res, Call) ->
	%?debugFmt("~n=== NOOP next state ===~n = State =~n~p~n = Res =~n~p~n = Call =~n~p", [State, Res, Call]),
	State.

decode_json_body({ok, _, _, Body}) ->
	jsx:to_term(list_to_binary(Body)).

insert_layer(Layer, Next, List) when is_atom(Next) ->
	List ++ [Layer];
insert_layer(Layer, Next, List) ->
	{Head, Tail} = lists:split(Next - 1, List),
	Head ++ [Layer] ++ Tail.

delete_layer(Nth, List) ->
	{Head, [_Nix | Tail]} = lists:split(Nth -1, List),
	Head ++ Tail.

%insert_layer_before(Obj, BeforeObj, List) when is_atom(BeforeObj) ->
%	List ++ [Obj];
%insert_layer_before(Obj, BeforeObj, List) ->
%	insert_layer_before(Obj, BeforeObj, List, []).

%insert_layer_before(Obj, BeforeObj, [BeforeObj | Tail], Wound) ->
%	unwind([Obj, BeforeObj | Tail], Wound);
%insert_layer_before(Obj, BeforeObj, [Head | Tail], Wound) ->
%	insert_layer_before(Obj, BeforeObj, Tail, [Head | Wound]).

%unwind(Nacc, []) ->
%	Nacc;
%unwind(Nacc, [H | T]) ->
%	unwind([H | Nacc], T).

%% =======================================================
%% tests proper
%% =======================================================

create_bad_user(Name, Next, State) ->
	Json = make_json(Name, Next, State),
	ibrowse:send_req(?layer_url, [?badcookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

create_blank_name(Next, State) ->
	Json = make_json(<<>>, Next, State),
	ibrowse:send_req(?layer_url, [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

create_name_conflict(Existant, Next, State) ->
	ExistantObj = lists:nth(Existant, State),
	Name = proplists:get_value(<<"name">>, ExistantObj),
	Json = make_json(Name, Next, State),
	ibrowse:send_req(?layer_url, [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

create_bad_map_id(Name, Next, MapId, State) ->
	Json = make_json(Name, Next, State),
	Url = "http://localhost:9097/map/" ++ integer_to_list(MapId) ++ "/layers",
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

create(Name, Next, State) ->
	Json = make_json(Name, Next, State),
	Out = ibrowse:send_req(?layer_url, [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)),
	TestFun = fun() ->
			{ok, Gots} = rpgb_data:search(rpgb_rec_layer, [{battlemap_id, 9000}]),
			length(Gots) == ( length(State) + 1 )
	end,
	rpgb_test_util:wait_until(TestFun),
	Out.

get_layers() ->
	ibrowse:send_req(?layer_url, [?cookie, ?accepts, ?contenttype], get, []).

get_a_layer(LayerNth, State) ->
	Layer = lists:nth(LayerNth, State),
	Url = proplists:get_value(<<"url">>, Layer),
	ibrowse:send_req(binary_to_list(Url), [?cookie, ?accepts, ?contenttype], get, []).

update_bad_user(Name, Next, ExistantN, State) ->
	Existant = lists:nth(ExistantN, State),
	Json = make_json(Name, Next, State),
	ibrowse:send_req(?layer_url(Existant), [?badcookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

update_blank_name(Next, ExistN, State) ->
	Exist = lists:nth(ExistN, State),
	Json = make_json(<<>>, Next, State),
	ibrowse:send_req(?layer_url(Exist), [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

update(Name, Next, LayerN, State) ->
	Layer = lists:nth(LayerN, State),
	Json = make_json(Name, Next, State),
	ibrowse:send_req(?layer_url(Layer), [?contenttype, ?accepts, ?cookie], put, jsx:to_json(Json)).

update_bad_reorder(BadNext, LayerN, State) ->
	Layer = lists:nth(LayerN, State),
	NextId = case BadNext of
		self ->
			proplists:get_value(<<"id">>, Layer);
		_ ->
			BadNext
	end,
	Json = [{next_layer_id, NextId}],
	ibrowse:send_req(?layer_url(Layer), [?contenttype, ?accepts, ?cookie], put, jsx:to_json(Json)).

delete_bad_user(N, State) ->
	Layer = lists:nth(N, State),
	ibrowse:send_req(?layer_url(Layer), [?contenttype, ?badcookie, ?accepts], delete, []).

delete_last_layer(LayerN, State) ->
	Layer = lists:nth(LayerN, State),
	ibrowse:send_req(?layer_url(Layer), [?contenttype, ?cookie, ?accepts], delete, []).

delete(LayerN, Layers) ->
	Layer = lists:nth(LayerN, Layers),
	ibrowse:send_req(?layer_url(Layer), [?contenttype, ?cookie, ?accepts], delete, []).

make_json(undefined, undefined, _) ->
	[{}];
make_json(Name, undefined, _) ->
	[{name, Name}];
make_json(undefined, null, _State) ->
	[{next_layer_id, null}];
make_json(Name, null, _State) ->
	[{next_layer_id, null}, {name, Name}];
make_json(undefined, Next, State) ->
	Obj = lists:nth(Next, State),
	NextId = proplists:get_value(<<"id">>, Obj),
	[{next_layer_id, NextId}];
make_json(Name, Next, State) ->
	BaseJson = make_json(undefined, Next, State),
	[{name, Name} | BaseJson].

case_url_type(_,_) -> ok.
make_json(_,_) -> ok.

%% =======================================================
%% postcondition
%% =======================================================

postcondition(_Layers, {call, _, create, [Name, NextId, _UrlType]}, {ok, "201", Heads, Body}) ->
	Location = proplists:get_value("Location", Heads),
	?assertNotEqual(undefined, Location),
	BodyJson = jsx:to_term(list_to_binary(Body)),
	?assertEqual(list_to_binary(Location), proplists:get_value(<<"url">>, BodyJson)),
	?assertEqual(Name, proplists:get_value(<<"name">>, BodyJson)),
	true;

postcondition(_Layers, {call, _, create_bad_user, _}, {ok, "403", _, _}) ->
	true;

postcondition(_Layers, {call, _, create_blank_name, _}, {ok, "422", _, _}) ->
	true;

postcondition(_Layers, {call, _, create_name_conflict, _}, {ok, "409", _, _}) ->
	true;

postcondition(_Layers, {call, _, create_missing_map_id, _}, {ok, "404", _, _}) ->
	true;

postcondition(_Layers, {call, _, create_bad_map_id, _}, {ok, "404", _, _}) ->
	true;

postcondition(Layers, {call, _, get_layers, _}, {ok, "200", _, Body}) ->
	BodyJson = jsx:to_term(list_to_binary(Body)),
	assert_layer_list(Layers, BodyJson);

postcondition(Layers, {call, _, get_a_layer, [ExistantN, _State]}, {ok, "200", _, Body}) ->
	BodyJson = jsx:to_term(list_to_binary(Body)),
	Existant = lists:nth(ExistantN, Layers),
	assert_layer(Existant, BodyJson),
	true;

postcondition(_Layers, {call, _, update_bad_user, _}, {ok, "403", _, _}) ->
	true;

postcondition(_Layers, {call, _, update_blank_name, _}, {ok, "422", _, _}) ->
	true;

postcondition(Layers, {call, _, update, [Name, NextN, ExistantN, _State]}, {ok, "200", _, Body}) ->
	BodyJson = jsx:to_term(list_to_binary(Body)),
	Original = lists:nth(ExistantN, Layers),
	Expected = case Name of
		undefined ->
			Original;
		_ ->
			[{<<"name">>, Name} | proplists:delete(<<"name">>, Original)]
	end,
	Expected2 = case NextN of
		null ->
			proplists:delete(<<"next_layer_id">>, Expected);
		undefined ->
			if
				ExistantN < length(Layers) ->
					NextLayer = lists:nth(ExistantN + 1, Layers),
					NextId = proplists:get_value(<<"id">>, NextLayer),
					[{<<"next_layer_id">>, NextId} | proplists:delete(<<"next_layer_id">>, Expected)];
				true ->
					proplists:delete(<<"next_layer_id">>, Expected)
			end;
		_ ->
			NextLayer = lists:nth(NextN, Layers),
			NextId = proplists:get_value(<<"id">>, NextLayer),
			[{<<"next_layer_id">>, NextId} | proplists:delete(<<"next_layer_id">>, Expected)]
	end,
	assert_layer(Expected2, BodyJson),
	true;

postcondition(_Layers, {call, _, update_bad_reorder, _}, {ok, "422", _, _}) ->
	true;

postcondition(_Layers, {call, _, delete_bad_user, _}, {ok, "403", _, _}) ->
	true;

postcondition(_Layers, {call, _, delete_last_layer, _}, {ok, "422", _, _}) ->
	true;

postcondition(_Layers, {call, _, delete, _}, {ok, "204", _, _}) ->
	true;

postcondition(Layers, Call, Res) ->
	?debugFmt("~n=== Postcondition fall through ===~n=== Layers:~n~p~n=== Call:~n~p~n=== Res:~n~p", [Layers, Call, Res]),
	false.

assert_layer_list(Expected, Got) ->
	?assertEqual(length(Expected), length(Got)),
	assert_layer_list_(Expected, Got).

assert_layer_list_([], []) ->
	true;
assert_layer_list_([EHead, ENext | ETail], [GH | GT]) ->
	EHead1 = proplists:delete(<<"next_layer_id">>, EHead),
	NextId = proplists:get_value(<<"id">>, ENext),
	EHead2 = [{<<"next_layer_id">>, NextId} | EHead1],
	assert_layer(EHead2, GH),
	assert_layer_list_([ENext | ETail], GT);
assert_layer_list_([EH | ET], [GH | GT]) ->
	EH1 = proplists:delete(<<"next_layer_id">>, EH),
	assert_layer(EH1, GH),
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
	update_existant(T, [{K, V} | Acc2]);
update_existant([Key | Tail], Acc) ->
	Acc2 = proplists:delete(Key, Acc),
	update_existant(Tail, Acc2).