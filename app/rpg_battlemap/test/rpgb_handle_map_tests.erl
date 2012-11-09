-module(rpgb_handle_map_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-define(cookie, begin
	{Head, Cookie} = cowboy_cookies:cookie(<<"rpgbsid">>, <<"sessionid">>),
	{"Cookie", binary_to_list(Cookie)}
end).
-define(accepts, {"Accept", "application/json"}).
-define(contenttype, {"Content-Type", "application/json"}).

-compile(export_all).

-record(state, {url, props}).

browser_test_() -> {setup, fun() ->
		rpgb_test_util:web_test_setup(?MODULE),
		rpgb_test_util:create_authed_session(<<"sessionid">>)
	end,
	fun(_) ->
		rpgb_test_util:web_test_teardown()
	end,
	fun(_) -> [

		{"Simple create", fun() ->
			Url = "http://localhost:9093/map",
			Binary = jsx:to_json([{gridline_color, <<"pink">>}]),
			Res = ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, Binary),
			?assertMatch({ok, "201", _H, _B}, Res),
			{ok, _S, Heads, Body} = Res,
			?assertMatch("http://localhost:9093/map/" ++ _MapId, proplists:get_value("Location", Heads))
		end},

		{"statem", timeout, 60000, fun() ->
			?assert(proper:quickcheck(?MODULE:prop_map_statem()))
		end}

	] end}.

prop_map_statem() ->
	?FORALL(Cmds, commands(?MODULE), begin
		{Hist, State, Res} = run_commands(?MODULE, Cmds),
		?WHENFAIL(?debugFmt("proper check failed!\n== Hstory ==\n~p\n\n== State ==\n~p\n\n== Result ==\n~p", [Hist, State, Res]),
			Res == ok)
	end).

%% =======================================================
%% generators
%% =======================================================

initial_state() ->
	#state{}.

command(#state{url = undefined} = State) ->
	{call, ?MODULE, create_map, [rpgb_prop:g_mapjson(), State]};
command(State) ->
	frequency([
		{1, {call, ?MODULE, destroy_map, [State]}},
		{9, {call, ?MODULE, update_map, [rpgb_prop:g_mapjson(), State]}}
	]).

%% =======================================================
%% preconditions
%% =======================================================

precondition(#state{url = undefined}, {call, _, create_map, _}) ->
	true;
precondition(_S, {call, _, create_map, _}) ->
	false;
precondition(#state{url = undefined}, _Blorp) ->
	false;
precondition(S, Blorp) ->
	true.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, Result, {call, _, create_map, _}) ->
	State#state{
		url = {call, ?MODULE, extract_location_header, [Result]},
		props = {call, ?MODULE, decode_json_body, [Result]}
	};

next_state(State, _Result, {call, _, destroy_map, _}) ->
	State#state{url = undefined};

next_state(State, _Res, _Call) ->
	State.

extract_location_header({ok, _State, Headers, _Body} = R) ->
	proplists:get_value("Location", Headers);

extract_location_header(Res) ->
	{call, proplists, get_value, ["Location",
		{call, erlang, element, [3, Res]}
	]}.

decode_json_body({ok, _State, _Headers, Body}) ->
	jsx:to_term(list_to_binary(Body)).

%% =======================================================
%% tests proper
%% =======================================================

create_map(Json, #state{url = undefined}) ->
	Url = "http://localhost:9093/map",
	Binary = jsx:to_json(case Json of [] -> [{}]; _ -> Json end),
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, Binary).

destroy_map(#state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts], delete).

update_map(Json, #state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

%% =======================================================
%% postcondition
%% =======================================================

postcondition(State, {call, _, create_map, [Json, InState]}, {ok, "201", Heads, Body} = Goop) ->
	Location = proplists:get_value("Location", Heads),
	?assertNotEqual(undefined, Location),
	BodyJson = jsx:to_term(list_to_binary(Body)),
	?assertEqual(list_to_binary(Location), proplists:get_value(<<"url">>, BodyJson)),
	?assert(assert_body(Json, Body)),
	true;

postcondition(State, {call, _, update_map, [Json, InState]}, {ok, "200", Heads, Body}) ->
	assert_body(Json, Body);

postcondition(State, {call, _, destroy_map, [_InState]}, {ok, "204", Heads, Body}) ->
	true;

postcondition(State, Call, Res) ->
	false.

assert_body(Json, Body) when is_list(Body) ->
	assert_body(Json, list_to_binary(Body));

assert_body(Json, Body) ->
	Json1 = binary_keys(Json),
	BodyJson = jsx:to_term(Body),
	match_keys(lists:sort(Json1), lists:sort(BodyJson)).

binary_keys(L) ->
	binary_keys(L, []).

binary_keys([], Acc) ->
	lists:reverse(Acc);

binary_keys([{K, V} | Tail], Acc) when is_atom(K) ->
	binary_keys(Tail, [{list_to_binary(atom_to_list(K)), V} | Acc]);

binary_keys([{K, _} = H | Tail], Acc) when is_binary(K) ->
	binary_keys(Tail, [H | Acc]).

match_keys([], _) ->
	true;
match_keys(_, []) ->
	false;
match_keys([{Key, Val} | ETail], [{Key, Val} | GTail]) ->
	match_keys(ETail, GTail);
match_keys(Expected, [Got | Tail]) ->
	match_keys(Expected, Tail).
