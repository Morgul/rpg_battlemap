-module(rpgb_handle_map_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-define(create_url, "http://localhost:9093/map").
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

-record(state, {url, props}).

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
	#state{}.

command(#state{url = undefined} = State) ->
	oneof([
		{call, ?MODULE, bad_user_create, [rpgb_prop:g_mapjson(), State]},
		{call, ?MODULE, create_blank_name, [rpgb_prop:g_mapjson(), State]},
		{call, ?MODULE, create_missing_name, [?SUCHTHAT(X, rpgb_prop:g_mapjson(), begin proplists:get_value(name, X) == undefined end), State]},
		{call, ?MODULE, create_map, [?SUCHTHAT(X, rpgb_prop:g_mapjson(), begin proplists:get_value(name, X) =/= undefined end), State]}
	]);
command(State) ->
	frequency([
		{9, {call, ?MODULE, simple_get, [State]}},
		{9, {call, ?MODULE, bad_user_destroy, [State]}},
		{9, {call, ?MODULE, create_name_conflict, [rpgb_prop:g_mapjson(), State]}},
		{9, {call, ?MODULE, update_map, [rpgb_prop:g_mapjson(), State]}},
		{9, {call, ?MODULE, bad_user_update, [rpgb_prop:g_mapjson(), State]}},
		{9, {call, ?MODULE, update_blank_name, [rpgb_prop:g_mapjson(), State]}},
		{1, {call, ?MODULE, destroy_map, [State]}}
	]).

%% =======================================================
%% preconditions
%% =======================================================

precondition(#state{url = Url}, {call, _, Function, _}) ->
	TheyUseNoMap = [create_map, bad_user_create, create_blank_name, create_missing_name],
	UsesNoMap = lists:member(Function, TheyUseNoMap),
	case {Url, UsesNoMap} of
		{undefined, true} -> true; % no map defined, don't need one
		{undefined, false} -> false; % no map defined, need one
		{_, true} -> false; % map defined, don't need one
		_ -> true % map defined, need one
	end.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, {ok, _, _, _} = Res, {call, _, create_map, _}) ->
	State#state{
		url = extract_location_header(Res),
		props = decode_json_body(Res)
	};

next_state(State, Result, {call, _, create_map, _}) ->
	State#state{
		url = {call, ?MODULE, extract_location_header, [Result]},
		props = {call, ?MODULE, decode_json_body, [Result]}
	};

next_state(State, _Result, {call, _, destroy_map, _}) ->
	State#state{url = undefined};

next_state(State, {ok, _, _, _} = Res, {call, _, update_map, _}) ->
	State#state{
		props = decode_json_body(Res)
	};

next_state(State, Result, {call, _, update_map, _}) ->
	State#state{
		props = {call, ?MODULE, decode_json_body, [Result]}
	};

next_state(State, _Res, _Call) ->
	State.

extract_location_header({ok, _State, Headers, _Body}) ->
	proplists:get_value("Location", Headers);

extract_location_header(Res) ->
	{call, proplists, get_value, ["Location",
		{call, erlang, element, [3, Res]}
	]}.

decode_json_body({ok, _State, _Headers, Body}) ->
	jsx:to_term(list_to_binary(Body));

decode_json_body(Res) ->
	{call, jsx, to_term, [
		{call, erlang, list_to_binary, [
			{call, erlang, element, [3, Res]}
		]}
	]}.

%% =======================================================
%% tests proper
%% =======================================================

%% commands with no map yet
create_map(Json, #state{url = undefined}) ->
	Binary = jsx:to_json(case Json of [] -> [{}]; _ -> Json end),
	ibrowse:send_req(?create_url, [?cookie, ?accepts, ?contenttype], put, Binary).

bad_user_create(Json, #state{url = undefined}) ->
	Binary = jsx:to_json(Json),
	ibrowse:send_req(?create_url, [?accepts, ?contenttype], put, Binary).

create_blank_name([{}], State) ->
	create_blank_name([], State);

create_blank_name(Json, #state{url = undefined}) ->
	Json2 = [{name, <<>>} | proplists:delete(name, Json)],
	Binary = jsx:to_json(Json2),
	ibrowse:send_req(?create_url, [?cookie, ?accepts, ?contenttype], put, Binary).

create_missing_name(Json, _State) ->
	Binary = jsx:to_json(Json),
	ibrowse:send_req(?create_url, [?cookie, ?accepts, ?contenttype], put, Binary).

%% commands with a map
destroy_map(#state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts], delete).

simple_get(#state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts], get).

bad_user_destroy(#state{url = Url}) ->
	ibrowse:send_req(Url, [?badcookie, ?accepts], delete).

create_name_conflict([{}], State) ->
	create_name_conflict([], State);

create_name_conflict(Json, #state{props = Props}) ->
	Name = proplists:get_value(<<"name">>, Props),
	Json2 = [{name, Name} | proplists:delete(name, Json)],
	Binary = jsx:to_json(Json2),
	ibrowse:send_req(?create_url, [?accepts, ?cookie, ?contenttype], put, Binary).

update_map(Json, #state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

bad_user_update(Json, #state{url = Url}) ->
	Binary = jsx:to_json(Json),
	ibrowse:send_req(Url, [?badcookie, ?accepts, ?contenttype], put, Binary).

update_blank_name([{}], State) ->
	update_blank_name([], State);

update_blank_name(Json, #state{url = Url}) ->
	Json2 = [{name, <<>>} | proplists:delete(name, Json)],
	Binary = jsx:to_json(Json2),
	ibrowse:send_req(Url, [?accepts, ?contenttype, ?cookie], put, Binary).

%% =======================================================
%% postcondition
%% =======================================================

postcondition(_State, {call, _, create_map, [Json, _InState]}, {ok, "201", Heads, Body}) ->
	Location = proplists:get_value("Location", Heads),
	?assertNotEqual(undefined, Location),
	BodyJson = jsx:to_term(list_to_binary(Body)),
	?assertEqual(list_to_binary(Location), proplists:get_value(<<"url">>, BodyJson)),
	?assert(rpgb_test_util:assert_body(Json, Body)),
	Layers = proplists:get_value(<<"layers">>, BodyJson),
	?assertMatch([[{_, _} | _]], Layers),
	[BottomLayer] = Layers,
	?assertEqual(<<"Bottom Layer">>, proplists:get_value(<<"name">>, BottomLayer)),
	true;

postcondition(_State, {call, _, bad_user_create, _}, {ok, "401", _Heads, _Body}) ->
	true;

postcondition(_State, {call, _, create_blank_name, _}, {ok, "422", _Heads, Body}) ->
	Json = jsx:to_term(list_to_binary(Body)),
	?assertEqual(<<"name cannot be blank.">>, Json),
	true;

postcondition(_State, {call, _, create_missing_name, _}, {ok, "422", _Heads, Body}) ->
	Json = jsx:to_term(list_to_binary(Body)),
	?assertEqual(<<"name cannot be blank.">>, Json),
	true;

postcondition(_State, {call, _, destroy_map, [_InState]}, {ok, "204", _Heads, _Body}) ->
	true;

postcondition(State, {call, _, simple_get, [_InState]}, {ok, "200", _Heads, Body}) ->
	?assert(rpgb_test_util:assert_body(State#state.props, Body)),
	true;

postcondition(_State, {call, _, bad_user_destroy, _}, {ok, "403", _Heads, _Body}) ->
	true;

postcondition(_State, {call, _, create_name_conflict, _}, {ok, "409", _Heads, Body}) ->
	Json = jsx:to_term(list_to_binary(Body)),
	?assertEqual(<<"you already have a map by that name.">>, Json),
	true;

postcondition(_State, {call, _, update_map, [Json, _InState]}, {ok, "200", _Heads, Body}) ->
	?assert(rpgb_test_util:assert_body(Json, Body)),
	true;

postcondition(_State, {call, _, bad_user_update, _}, {ok, "403", _Head, _Body}) ->
	true;

postcondition(_State, {call, _, update_blank_name, _}, {ok, "422", _Head, Body}) ->
	Json = jsx:to_term(list_to_binary(Body)),
	?assertEqual(<<"name cannot be blank.">>, Json),
	true;

postcondition(_State, _Call, _Res) ->
	false.
