-module(rpgb_handle_character_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

-define(create_url, "http://localhost:" ++ integer_to_list(rpgb_test_util:get_port(?MODULE)) ++ "/character").
-define(cookie, begin
	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"sessionid">>),
	{"Cookie", Cookie}
end).
-define(badcookie, begin
	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"baduser">>),
	{"Cookie", Cookie}
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
			?assert(proper:quickcheck(?MODULE:prop_character_statem()))
		end}

	] end}.

prop_character_statem() ->
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
		{call, ?MODULE, bad_user_create, [rpgb_prop:g_characterjson(), State]},
		{call, ?MODULE, create_blank_name, [rpgb_prop:g_characterjson(), State]},
		{call, ?MODULE, create_missing_name, [?SUCHTHAT(X, rpgb_prop:g_characterjson(), begin proplists:get_value(name, X) == undefined end), State]},
		{call, ?MODULE, create_character, [?SUCHTHAT(X, rpgb_prop:g_characterjson(), begin proplists:get_value(name, X) =/= undefined end), State]}
	]);
command(State) ->
	frequency([
		{9, {call, ?MODULE, simple_get, [State]}},
		{9, {call, ?MODULE, other_user_get, [State]}},
		{9, {call, ?MODULE, bad_user_destroy, [State]}},
		{9, {call, ?MODULE, create_name_conflict, [rpgb_prop:g_characterjson(), State]}},
		{9, {call, ?MODULE, update_character, [rpgb_prop:g_characterjson(), State]}},
		{9, {call, ?MODULE, bad_user_update, [rpgb_prop:g_characterjson(), State]}},
		{9, {call, ?MODULE, update_blank_name, [rpgb_prop:g_characterjson(), State]}},
		{1, {call, ?MODULE, destroy_character, [State]}}
	]).

%% =======================================================
%% preconditions
%% =======================================================

precondition(#state{url = Url}, {call, _, Function, _}) ->
	TheyUseNoChar = [bad_user_create, create_blank_name, create_missing_name, create_character],
	UseNoChar = lists:member(Function, TheyUseNoChar),
	case {Url, UseNoChar} of
		{undefined, true} -> true;
		{undefined, false} -> false;
		{_, true} -> false;
		_ -> true
	end.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, Result, {call, _, create_character, _}) ->
	State#state{
		url = {call, ?MODULE, extract_location_header, [Result]},
		props = {call, ?MODULE, decode_json_body, [Result]}
	};

next_state(State, _Result, {call, _, destroy_character, _}) ->
	State#state{url = undefined, props = undefined};

next_state(State, Result, {call, _, update_character, _}) ->
	State#state{
		props = {call, ?MODULE, decode_json_body, [Result]}
	};

next_state(State, Result, {call, _, simple_get, _Args}) ->
	State#state{
		props = {call, ?MODULE, decode_json_body, [Result]}
	};

next_state(State, _Result, {call, _, other_user_get, _Args}) ->
	State;

next_state(State, _Res, _Call) ->
	State.

extract_location_header({ok, _State, Headers, _Body}) ->
	proplists:get_value("location", Headers).

decode_json_body({ok, _State, _Headers, Body}) ->
	jsx:to_term(list_to_binary(Body)).

%% =======================================================
%% tests proper
%% =======================================================

%% commands with no character yet
create_character([], State) ->
	create_character([{}], State);
create_character(Json, _State) ->
	Binary = jsx:to_json(Json),
	ibrowse:send_req(?create_url, [?cookie, ?accepts, ?contenttype], put, Binary).

bad_user_create(Json, #state{url = undefined}) ->
	Binary = jsx:to_json(Json),
	ibrowse:send_req(?create_url, [?accepts, ?contenttype], put, Binary).

create_blank_name([{}], State) ->
	create_blank_name([], State);
create_blank_name(Json, _State) ->
	Json2 = [{name, <<>>} | proplists:delete(name, Json)],
	Binary = jsx:to_json(Json2),
	ibrowse:send_req(?create_url, [?cookie, ?accepts, ?contenttype], put, Binary).

create_missing_name(Json, _State) ->
	Binary = jsx:to_json(Json),
	ibrowse:send_req(?create_url, [?cookie, ?accepts, ?contenttype], put, Binary).

%% commands with a character
destroy_character(#state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts], delete).

simple_get(#state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts], get).

other_user_get(#state{url = Url}) ->
	ibrowse:send_req(Url, [?accepts, ?badcookie], get).

bad_user_destroy(#state{url = Url}) ->
	ibrowse:send_req(Url, [?badcookie, ?accepts], delete).

create_name_conflict([{}], State) ->
	create_name_conflict([], State);

create_name_conflict(Json, #state{props = Props}) ->
	Name = proplists:get_value(<<"name">>, Props),
	Json2 = [{name, Name} | proplists:delete(name, Json)],
	Binary = jsx:to_json(Json2),
	ibrowse:send_req(?create_url, [?accepts, ?cookie, ?contenttype], put, Binary).

update_character([], State) ->
	update_character([{}], State);
update_character(Json, #state{url = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts, ?contenttype], put, jsx:to_json(Json)).

bad_user_update([], State) ->
	bad_user_update([{}], State);
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

postcondition(_State, {call, _, create_character, [Json, _InState]}, {ok, "201", Heads, Body}) ->
	Location = proplists:get_value("location", Heads),
	?assertNotEqual(undefined, Location),
	BodyJson = jsx:to_term(list_to_binary(Body)),
	?assertEqual(list_to_binary(Location), proplists:get_value(<<"url">>, BodyJson)),
	?assert(rpgb_test_util:assert_body(Json, Body)),
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

postcondition(_State, {call, _, destroy_character, [_InState]}, {ok, "204", _Heads, _Body}) ->
	true;

postcondition(State, {call, _, simple_get, [_InState]}, {ok, "200", _Heads, Body}) ->
	?assert(rpgb_test_util:assert_body(State#state.props, Body)),
	true;

postcondition(State, {call, _, other_user_get, [_InState]}, {ok, Status, _Heads, Body}) ->
	case proplists:get_value(<<"public">>, State#state.props) of
		true ->
			?assertEqual("200", Status),
			?assert(rpgb_test_util:assert_body(State#state.props, Body)),
			true;
		_Else ->
			?assertEqual("403", Status),
			true
	end;

postcondition(_State, {call, _, bad_user_destroy, _}, {ok, "403", _Heads, _Body}) ->
	true;

postcondition(_State, {call, _, create_name_conflict, _}, {ok, "409", _Heads, Body}) ->
	Json = jsx:to_term(list_to_binary(Body)),
	?assertEqual(<<"you already have a character by that name.">>, Json),
	true;

postcondition(_State, {call, _, update_character, [Json, _InState]}, {ok, "200", _Heads, Body}) ->
	?assert(rpgb_test_util:assert_body(Json, Body)),
	true;

postcondition(_State, {call, _, bad_user_update, _}, {ok, "403", _Head, _Body}) ->
	true;

postcondition(_State, {call, _, update_blank_name, _}, {ok, "422", _Head, Body}) ->
	Json = jsx:to_term(list_to_binary(Body)),
	?assertEqual(<<"name cannot be blank.">>, Json),
	true;

postcondition(_State, _Call, _Res) ->
	?debugFmt("Fall through postcondition~n"
		"    State: ~p~n"
		"    Call: ~p~n"
		"    Res: ~p", [_State, _Call, _Res]),
	false.
