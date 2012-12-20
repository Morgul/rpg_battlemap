-module(rpgb_handle_combatant_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-define(combatant_url, "http://localhost:9095/map/9000/combatants").
-define(layer_combatants(LayerId), "http://localhost:9095/map/9000/layer/" ++ integer_to_list(LayerId) ++ "/combatants"),
-define(combatant_url(CombatantId), if is_integer(CombatantId) -> ?combatant_url ++ "/" ++ integer_to_list(CombatantId); true -> binary_to_list(proplists:get_value(<<"url">>, CombatantId)) end).
-define(mapid, 9000).
-define(layer1id, 3001).
-define(layer2id, 3002).
-define(layer3id, 3003).
-define(cookie, begin
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"sessionid">>),
	{"Cookie", binary_to_list(Cookie)}
end).
-define(participant, begin
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"participant">>),
	{"Cookie", binary_to_list(Cookie)}
end).
-define(badcookie, begin
	{_Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"baduser">>),
	{"Cookie", binary_to_list(Cookie)}
end).
-define(accepts, {"Accepts", "application/json"}).
-define(contenttype, {"Content-Type", "application/json"}).

-compile(export_all).

browser_test_() -> {setup, fun() ->
		rpgb_test_util:web_test_setup(?MODULE)
	end,
	fun(_) ->
		rpgb_test_util:web_test_teardown()
	end,
	fun(_) -> [

		{"splice", [
			?_assertEqual([1], splice([], 1, 0, [1])),
			?_assertEqual([], splice([1], 1, 1, [])),
			?_assertEqual([1,2,3], splice([1,3], 2, 0, [2])),
			?_assertEqual([1,3], splice([1,2,3], 2, 1, [])),
			?_assertEqual([1,a,3], splice([1,2,3], 2, 1, [a])),
			?_assertEqual([1,2,3,4,5,6,7,8,9], splice([1,2,3,7,8,9], 4, 0, [4,5,6]))
		]},

		{"statem", timeout, 60000, fun() ->
			?assert(proper:quickcheck(?MODULE:prop_map_statem()))
		end}

	] end}.

prop_map_statem() ->
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
	rpgb_test_util:create_authed_session(<<"sessionid">>, <<"map_owner">>),
	rpgb_test_util:create_authed_session(<<"baduser">>, <<"explodeier">>),
	rpgb_test_util:create_authed_session(<<"participant">>, <<"partier">>),
	{ok, UserSession} = rpgb_session:get(<<"sessionid">>),
	{ok, PartierSession} = rpgb_session:get(<<"participant">>),
	User = rpgb_session:get_user(UserSession),
	Partier = rpgb_session:get_user(PartierSession),
	[rpgb_data:save(#rpgb_rec_layer{
		name = list_to_binary("layer " ++ integer_to_list(N)),
		id = N + 3000,
		battlemap_id = 9000,
		next_layer_id = if N > 2 -> 3000 + N; true -> undefined end
	}) || N <- lists:seq(1,3)],
	Map = #rpgb_rec_battlemap{
		id = 9000,
		owner_id = User#rpgb_rec_user.id,
		participant_ids = [Partier#rpgb_rec_user.id],
		bottom_layer_id = 3001
	},
	{ok, Map2} = rpgb_data:save(Map),
	[].


command(S) ->
	oneof([
		{call, ?MODULE, create, [?SUCHTHAT(X, g_combatant(), begin Name = proplists:get_value(<<"name">>, X), Name =/= undefined andalso Name =/= <<>> end), g_next(S), g_creator(), S]},
		{call, ?MODULE, create_batch, [?SUCHTHAT(X, g_combatant(), begin Name = proplists:get_value(<<"name">>, X), Name =/= undefined andalso Name =/= <<>> end), g_next(S), g_creator(), choose(1, 20), S]},
		%{call, ?MODULE, create_bad_batch, [g_combatant(), g_next(S), g_bad_batch(), g_creator(), S]},
		%{call, ?MODULE, create_bad_user, [g_combatant(), g_existant(S), S]},
		%{call, ?MODULE, create_blank_name, [g_combatant(), g_existant(S), g_creator(), S]},
		%{call, ?MODULE, create_bad_map_id, [g_combatant(), g_existant(S), g_creator(), S]},
		%{call, ?MODULE, create_bad_layer_id, [g_combatant(), g_existant(S), g_creator(), S]},
		{call, ?MODULE, update, [g_combatant(), g_existant(S), g_next(S), g_maybe_layer(), g_creator(), S]},
		%{call, ?MODULE, update_bad_user, [g_combatant(), g_existant(S), S]},
		%{call, ?MODULE, update_blank_name, [g_combatant(), g_existant(S), g_creator(), S]},
		%{call, ?MODULE, update_bad_reorder, [oneof([self, choose(90000, 100000)]), g_combatant(), g_existant(S), g_creator(), S]},
		%{call, ?MODULE, update_bad_layer, [g_combatant(), g_existant(S), S]},
		{call, ?MODULE, get_a_combatant, [g_creator(), g_existant(S), S]},
		%{call, ?MODULE, get_layer_combatants, [g_creator(), choose(3001, 3003)]},
		{call, ?MODULE, get_map_combatants, [g_creator()]},
		%{call, ?MODULE, delete_bad_user, [g_existant(S), S]},
		{call, ?MODULE, delete, [g_existant(S), S]}
	]).

g_bad_batch() ->
	oneof([null, <<"not int">>, false, true, choose(21, 10000000), choose(-100000000, 0)]).

g_maybe_layer() ->
	oneof([null, undefined, 3001, 3002, 3003]).

g_existant([]) ->
	undefined;
g_existant([_]) ->
	1;
g_existant(Combatants) ->
	Max = length(Combatants),
	choose(1, Max).

g_next([]) ->
	oneof([null, undefined]);
g_next(S) ->
	oneof([null, undefined, g_existant(S)]).

g_creator() ->
	oneof([owner, partier]).

g_combatant() ->
	?LET(X, list(g_combatant_field()), rpgb_prop:uniquify(X)).

g_combatant_field() ->
	oneof([
		{<<"name">>, rpgb_prop:g_name()},
		{<<"color">>, rpgb_prop:g_color()},
		{<<"portrait_image">>, rpgb_prop:g_url()},
		{<<"token_image">>, rpgb_prop:g_url()},
		{<<"x">>, integer()},
		{<<"y">>, integer()},
		{<<"initiative">>, pos_integer()},
		{<<"size">>, pos_integer()},
		{<<"aura_size">>, non_neg_integer()},
		{<<"aura_color">>, rpgb_prop:g_color()}
	]).

%% =======================================================
%% preconditions
%% =======================================================

precondition(S, {call, _, Call, _}) when Call == delete; Call == delete_bad_user;
		Call == get_a_combatant ->
	length(S) > 0;
precondition(S, {call, _, update, [_, Nth, Next, _, _, _]}) when Nth =/= Next, length(S) > 0 ->
	true;
precondition(S, {call, _, update, _}) ->
	false;
precondition(_, _) ->
	true.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, Res, {call, _, create, [Put, Next, _Creator, _State]}) ->
	insert_at({call, ?MODULE, decode_res, [Res]}, Next, State);

next_state(State, Res, {call, _, create_batch, [Put, Next, _Creator, Count, _S]}) ->
	Calls = [{call, ?MODULE, decode_res_at, [Res, Nth]} || Nth <- lists:seq(1, Count)],
	case Next of
		Atom when is_atom(Atom) ->
			State ++ Calls;
		_ ->
			splice(State, Next, 0, Calls)
	end;

next_state(State, Res, {call, _, update, [_Put, Nth, null, _Layer, _Creator, _S]}) ->
	State2 = snip(Nth, State),
	State2 ++ [{call, ?MODULE, decode_res, [Res]}];

next_state(State, Res, {call, _, update, [_Put, Nth, undefined, _Layer, _Creator, _S]}) ->
	splice(State, Nth, 1, [{call, ?MODULE, decode_res, [Res]}]);

next_state(State, Res, {call, _, update, [_Put, Nth, Next, _Layer, _Creator, _S]}) when Nth < Next ->
	{Sacrifice, Tail} = lists:split(Next - 1, State),
	{Head, [_ | Mid]} = lists:split(Nth - 1, Sacrifice),
	Head ++ Mid ++ [{call, ?MODULE, decode_res, [Res]}] ++ Tail;

next_state(State, Res, {call, _, update, [_Put, Nth, Next, _Layer, _Creator, _S]}) when Next < Nth ->
	{Sacrifice, [_ | Tail]} = lists:split(Nth - 1, State),
	{Head, Mid} = lists:split(Next - 1, Sacrifice),
	Head ++ [{call, ?MODULE, decode_res, [Res]}] ++ Mid ++ Tail;

next_state(State, Res, {call, _, get_a_combatant, [_Who, Nth, _State]}) ->
	splice(State, Nth, 1, [{call, ?MODULE, decode_res, [Res]}]);

next_state(State, Res, {call, _, delete, [Nth, _State]}) ->
	snip(Nth, State);

next_state(State, _Result, _Call) ->
	State.

%% =======================================================
%% tests proper
%% =======================================================

create(Put, Next, Creator, State) ->
	SessCookie = case Creator of
		partier ->
			?participant;
		owner ->
			?cookie
	end,
	Json = make_json(Put, Next, State),
	{ok, "201", _, _} = Out = ibrowse:send_req(?combatant_url, [SessCookie, ?accepts, ?contenttype], put, jsx:to_json(Json)),
	TestFun = fun() ->
		{ok, Gots} = rpgb_data:search(rpgb_rec_combatant, [{battlemap_id, 9000}]),
		length(Gots) == (length(State) + 1)
	end,
	rpgb_test_util:wait_until(TestFun),
	Out.

create_batch(Put, Next, Creator, Count, State) ->
	SessCookie = case Creator of
		partier ->
			?participant;
		owner ->
			?cookie
	end,
	Json = make_json(Put, Next, State),
	Json2 = case Json of
		[{}] -> [{<<"batch">>, Count}];
		_ -> [{<<"batch">>, Count} | Json]
	end,
	{ok, Status, _, _} = Out = ibrowse:send_req(?combatant_url, [SessCookie, ?accepts, ?contenttype], put, jsx:to_json(Json2)),
	case Status of
		"200" ->
			ok;
		_ ->
			?debugFmt("that res:  ~p", [Out]),
			Status = "200"
	end,
	TestFun = fun() ->
		{ok, Gots} = rpgb_data:search(rpgb_rec_combatant, [{battlemap_id, 9000}]),
		length(Gots) == ( length(State) + Count)
	end,
	rpgb_test_util:wait_until(TestFun),
	Out.

create_bad_batch(Put, Next, Batch, Creator, State) ->
	SessCookie = get_session_cookie(Creator),
	Json = make_json(Put, Next, State),
	Json2 = case Json of
		[{}] -> [{<<"batch">>, Batch}];
		_ -> [{<<"batch">>, Batch} | Json]
	end,
	ibrowse:send_req(?combatant_url, [SessCookie, ?accepts, ?contenttype], put, jsx:to_json(Json2)).

update(Put, Nth, Next, LayerId, Who, State) ->
	SessCookie = case Who of
		partier ->
			?participant;
		owner ->
			?cookie
	end,
	Json1 = case make_json(Put, Next, State) of
		[{}] ->
			[];
		E ->
			E
	end,
	Combatant = lists:nth(Nth, State),
	Url = proplists:get_value(<<"url">>, Combatant),
	Json2 = case LayerId of
		undefined ->
			proplists:delete(<<"layer_id">>, Json1);
		_ ->
			[{<<"layer_id">>, LayerId} | proplists:delete(<<"layer_id">>, Json1)]
	end,
	Json3 = case Json2 of
		[] -> [{}];
		_ -> Json2
	end,
	ibrowse:send_req(binary_to_list(Url), [SessCookie, ?accepts, ?contenttype], put, jsx:to_json(Json3)).

get_a_combatant(Who, Nth, State) ->
	Combatant = lists:nth(Nth, State),
	Url = proplists:get_value(<<"url">>, Combatant),
	SessCookie = case Who of
		partier ->
			?participant;
		owner ->
			?cookie
	end,
	ibrowse:send_req(binary_to_list(Url), [SessCookie, ?accepts, ?contenttype], get, []).

get_map_combatants(Who) ->
	SessCookie = case Who of
		partier ->
			?participant;
		owner ->
			?cookie
	end,
	ibrowse:send_req(?combatant_url, [SessCookie, ?accepts], get, []).

delete(Nth, State) ->
	Combatant = lists:nth(Nth, State),
	Url = proplists:get_value(<<"url">>, Combatant),
	{ok, UserSession} = rpgb_session:get(<<"sessionid">>),
	{ok, PartierSession} = rpgb_session:get(<<"participant">>),
	User = rpgb_session:get_user(UserSession),
	Partier = rpgb_session:get_user(PartierSession),
	OwnerId = proplists:get_value(<<"owner_id">>, Combatant),
	UserCookie = if
		OwnerId == User#rpgb_rec_user.id ->
			?cookie;
		OwnerId == Partier#rpgb_rec_user.id ->
			?participant
	end,
	ibrowse:send_req(binary_to_list(Url), [?accepts, ?contenttype, UserCookie], delete, []).

%% =======================================================
%% postcondition
%% =======================================================

postcondition(State, {call, _, create, [Put, Next, Creator, _State]}, {ok, "201", _, Body}) ->
	%Json = jsx:to_term(list_to_binary(Body)),
	?assert(rpgb_test_util:assert_body(Put, Body)),
	true;

postcondition(State, {call, _, create_batch, [Put, Next, Creator, Batch, _State]}, {ok, "200", _, Body}) ->
	BaseName = proplists:get_value(<<"name">>, Put),
	Names = [begin Lint = list_to_binary(integer_to_list(N)), <<BaseName/binary, " ", Lint/binary>> end
	|| N <- lists:seq(1, Batch)],
	Puts = [[{<<"name">>, Name} | proplists:delete(<<"name">>, Put)] || Name <- Names],
	Got = jsx:to_term(list_to_binary(Body)),
	assert_batch(Puts, Got);

postcondition(State, {call, _, create_bad_batch, _}, {ok, "422", _, _}) ->
	true;

postcondition(State, {call, _, get_a_combatant, [_Who, Nth, _State]}, {ok, "200", _, Body}) ->
	Combatant = lists:nth(Nth, State),
	NextId = if
		Nth == length(State) ->
			null;
		true ->
			NextCombatant = lists:nth(Nth + 1, State),
			proplists:get_value(<<"id">>, NextCombatant)
	end,
	Expected = [{<<"next_combatant_id">>, NextId} | proplists:delete(<<"next_combatant_id">>, Combatant)],
	?assert(rpgb_test_util:assert_body(Expected, Body)),
	true;

postcondition(State, {call, _, get_map_combatants, _}, {ok, "200", _, Body}) ->
	State2 = fix_combatants_nexts(State),
	Got = jsx:to_term(list_to_binary(Body)),
	assert_batch(State2, Got);

postcondition(State, {call, _, update, [Put, Nth, Next, Layer, _Who, _S]}, {ok, "200", _, Body}) ->
	Combatant = lists:nth(Nth, State),
	Combatant1 = case Next of
		null ->
			lists:keyreplace(<<"next_combatant_id">>, 1, Combatant, {<<"next_combatant_id">>, null});
		undefined ->
			NextId = get_next_id(Nth, State),
			lists:keyreplace(<<"next_combatant_id">>, 1, Combatant, {<<"next_combatant_id">>, NextId});
		_ ->
			NextObj = lists:nth(Next, State),
			NextId = proplists:get_value(<<"id">>, NextObj),
			lists:keyreplace(<<"next_combatant_id">>, 1, Combatant, {<<"next_combatant_id">>, NextId})
	end,
	Combatant2 = case Layer of
		undefined ->
			Combatant1;
		_->
			lists:keyreplace(<<"layer_id">>, 1, Combatant1, {<<"layer_id">>, Layer})
	end,
	Combatant3 = update_keys(Combatant2, Put),
	?assert(rpgb_test_util:assert_body(Combatant3, Body)),
	true;

postcondition(State, {call, _, delete,[Nth, _State]}, {ok, "204", _, _}) ->
	true;

postcondition(S,C,R) ->
	?debugFmt("Catch all post condition:~n~p~n~p~n~p", [S,C,R]),
	false.

%% =======================================================
%% Internal
%% =======================================================

get_session_cookie(partier) -> ?participant;
get_session_cookie(owner) -> ?cookie.

get_next_id(Nth, List) when length(List) =< 1 ->
	null;
get_next_id(Nth, List) when Nth == length(List) ->
	null;
get_next_id(Nth, List) ->
	NextObj = lists:nth(Nth + 1, List),
	proplists:get_value(<<"id">>, NextObj).

update_keys(Proplist, []) ->
	Proplist;
update_keys(Proplist, [{}]) ->
	Proplist;
update_keys(Proplist, [{Key, Value} | Tail]) ->
	Proplist2 = lists:keyreplace(Key, 1, Proplist, {Key, Value}),
	update_keys(Proplist2, Tail).

fix_combatants_nexts([]) ->
	[];

fix_combatants_nexts(List) ->
	fix_combatants_nexts(List, []).

fix_combatants_nexts([E], Acc) ->
	E2 = lists:keyreplace(<<"next_combatant_id">>, 1, E, {<<"next_combatant_id">>, null}),
	lists:reverse([E2 | Acc]);

fix_combatants_nexts([E, Next | Tail], Acc) ->
	NextId = proplists:get_value(<<"id">>, Next),
	E2 = lists:keyreplace(<<"next_combatant_id">>, 1, E, {<<"next_combatant_id">>, NextId}),
	fix_combatants_nexts([Next | Tail], [E2 | Acc]).

assert_batch(Expected, Got) when length(Expected) =/= length(Got) ->
	?assertEqual(length(Expected), length(Got));

assert_batch([], []) ->
	true;

assert_batch([E | ETail], [G | GTail]) ->
	?assert(rpgb_test_util:match_keys(lists:sort(E), lists:sort(G))),
	assert_batch(ETail, GTail).

snip(Nth, List) ->
	{Head, [_ | Tail]} = lists:split(Nth - 1, List),
	Head ++ Tail.

splice(List, Start, Delete, Inserts) when is_integer(Start), is_integer(Delete),
		is_list(List), is_list(Inserts), Delete >= 0, Delete + Start - 1 =< length(List),
		Start >= 1, Delete >= 0 ->
	{Head, Tail} = lists:split(Start - 1, List),
	Nommed = delete_n(Tail, Delete),
	Head ++ Inserts ++ Nommed.

delete_n(List, 0) ->
	List;
delete_n([_ | List], N) when is_integer(N), N > 0 ->
	delete_n(List, N - 1).

insert_at(Insert, At, List) when is_atom(At) ->
	List ++ [Insert];
insert_at(Insert, At, List) ->
	{Head, Tail} = lists:split(At - 1, List),
	Head ++ [Insert] ++ Tail.

decode_res({ok, _Status, _Head, Body}) ->
	jsx:to_term(list_to_binary(Body)).

decode_res_at({ok, _State, _Head, Body}, Nth) ->
	Json = jsx:to_term(list_to_binary(Body)),
	lists:nth(Nth, Json).

make_json(Base, undefined, _State) ->
	Base;
make_json([{}], Next, State) ->
	make_json([], Next, State);
make_json(Base, null, _State) ->
	[{<<"next_combatant_id">>, null} | Base];
make_json(Base, Next, State) ->
	NextObj = lists:nth(Next, State),
	NextId = proplists:get_value(<<"id">>, NextObj),
	[{<<"next_combatant_id">>, NextId} | Base].

get_user(partier) ->
	get_user(<<"partier">>);
get_user(owner) ->
	get_user(<<"map_owner">>);
get_user(Username) ->
	{ok, [Out | _]} = rpgb_data:search(rpgb_rec_user, [{name, Username}]),
	Out.