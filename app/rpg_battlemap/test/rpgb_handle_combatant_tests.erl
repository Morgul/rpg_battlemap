-module(rpgb_handle_combatant_tests).

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
	% for the desired tests to work, I need in the data:
	% a map with an owner id different from participant
	% 3 layers
	#state{}.

command(#state{url = undefined} = State) ->
	oneof([
		{call, ?MODULE, participant_create_combatant, [rpgb_prop:g_combatantjson(), State]},
		{call, ?MODULE, owner_create_combatant, [rpgb_prop:g_combatantjson(), State]},
		{call, ?MODULE, create_no_map, [rpgb_prop:g_combatantjson(), State]},
		{call, ?MODULE, random_create_combatant, [rpgb_prop:g_combatantjson(), State]},
		{call, ?MODULE, create_no_layer, [rpgb_prop:g_combatantjson(), State]}
	]);
command(State) ->
	frequency([
		{1, {call, ?MODULE, owner_destroy_combatant, [State]}},
		{1, {call, ?MODULE, participant_destroy_combatant, [State]}},
		{5, {call, ?MODULE, other_destroy_combatant, [State]}},
		{5, {call, ?MODULE, move_to_nonexistant_layer, [State]}},
		{9, {call, ?MODULE, owner_update_combatant, [rpgb_prop:g_combatantjson(), State]}},
		{9, {call, ?MODULE, participant_update_combatant, [rpgb_prop:g_combatantjson(), State]}},
		{9, {call, ?MODULE, other_update_combatant, [rpgb_prop:g_combatantjson(), State]}}
	]).

%% =======================================================
%% preconditions
%% =======================================================

precondition(_, _) ->
	true.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, Result, Call) ->
	State.

%% =======================================================
%% tests proper
%% =======================================================

participant_create_combatant(Json, State) ->
	ok.

owner_create_combatant(Json, State) ->
	ok.

create_no_map(Json, State) ->
	ok.

random_create_combatant(Json, State) ->
	ok.

create_no_layer(Json, State) ->
	ok.

owner_destroy_combatant(State) ->
	ok.

participant_destroy_combatant(State) ->
	ok.

other_destroy_combatant(State) ->
	ok.

move_to_nonexistant_layer(Json, State) ->
	ok.

owner_update_combatant(Json, State) ->
	ok.

particiapnt_update_combatant(Json, State) ->
	ok.

other_udpate_combatant(Json, State) ->
	ok.

%% =======================================================
%% postcondition
%% =======================================================

postcondition(State, Call, Res) ->
	?assert(true),
	true.
