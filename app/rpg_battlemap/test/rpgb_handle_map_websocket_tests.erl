-module(rpgb_handle_map_websocket_tests).

-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

request_test_() ->
	{setup, fun() ->
		rpgb_test_util:web_test_setup(?MODULE),
		{ok, GoodUserSession} = rpgb_test_util:create_authed_session(<<"sessionid">>),
		rpgb_test_util:create_authed_session(<<"baduser">>),
		GoodUser = rpgb_session:get_user(GoodUserSession),
		Map = #rpgb_rec_battlemap{id = 1, name = <<"websocket test map">>,
			owner_id = GoodUser#rpgb_rec_user.id},
		{ok, Map2} = rpgb_data:save(Map),
		{GoodUser, Map2}
	end,
	fun(_) ->
		rpgb_test_util:web_test_teardown()
	end,
	fun(_) -> [

		{"connecting without session gets 401", fun() ->
			Got = gen_websocket:connect(ws_url(3), []),
			?assertEqual({error, {401, <<"Unauthorized">>}}, Got)
		end},

		{"connect to non-existant map", fun() ->
			Got = gen_websocket:connect(ws_url(3), [{headers, [bin_cookie(good)]}]),
			?assertEqual({error, {404, <<"Not Found">>}}, Got)
		end}%,

		%{"connect to a valid map", ?_assert(false)},

		%{"create a combatant", ?_assert(false)},

		%{"alerted that a combatant was created", ?_assert(false)},

		%{"update combatant", ?_assert(false)},

		%{"alerted that a combatant was updated", ?_assert(false)}

	] end}.

ws_url(MapId) ->
	"ws://localhost:9099/map/" ++ integer_to_list(MapId) ++ "/ws".

bin_cookie(V) ->
	{Head, Val} = cookie(V),
	{list_to_binary(Head), list_to_binary(Val)}.

cookie(good) ->
	Cookie =	 rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"sessionid">>),
	{"Cookie", Cookie}.

%-define(cookie, begin
%	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"sessionid">>),
%	{"Cookie", Cookie}
%end).
%-define(badcookie, begin
%	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"baduser">>),
%	{"Cookie", Cookie}
%end).
