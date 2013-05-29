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
	fun({_GoodUser, Map}) -> [

		{"connecting without session gets 401", fun() ->
			Got = gen_websocket:connect(ws_url(3), []),
			?assertEqual({error, {401, <<"Unauthorized">>}}, Got)
		end},

		{"connect to non-existant map", fun() ->
			Got = gen_websocket:connect(ws_url(3), [{headers, [bin_cookie(good)]}]),
			?assertEqual({error, {404, <<"Not Found">>}}, Got)
		end},

		{"has session, but doesn't own map", fun() ->
			Got = gen_websocket:connect(ws_url(Map#rpgb_rec_battlemap.id), [
				{headers, [bin_cookie(bad)]}
			]),
			?assertEqual({error, {403, <<"Forbidden">>}}, Got)
		end},

		{"connect to valid map endpoint", fun() ->
			Got = gen_websocket:connect(ws_url(Map#rpgb_rec_battlemap.id), [{headers, [bin_cookie(good)]}]),
			?assertMatch({ok, _WsSocket}, Got)
		end},

		{"communication tests", setup, fun() ->
			{ok, Ws} = gen_websocket:connect(ws_url(Map#rpgb_rec_battlemap.id), [{headers, [bin_cookie(good)]}]),
			Ws
		end,
		fun(Ws) ->
			gen_websocket:shutdown(Ws, normal)
		end,
		fun(Ws) -> [

			{"Change map name", fun() ->
				Request = [
					{<<"reply_with">>, 1},
					{<<"action">>, <<"put">>},
					{<<"type">>, <<"map">>},
					{<<"id">>, 1},
					{<<"data">>, [
						{<<"name">>, <<"ws map name changed">>}
					]}
				],
				Binary = jsx:to_json(Request),
				ok = gen_websocket:send(Ws, Binary, text),
				{ok, {text, Recv}} = gen_websocket:recv(Ws, 4000),
				Reply = jsx:to_term(Recv),
				?debugFmt("reply got:~n~p", [Reply]),
				?assertEqual(1, proplists:get_value(<<"reply_for">>, Reply))
				?assertEqual(true, proplists:get_value(<<"accepted">>, Reply))
			end}

		] end}

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
	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"sessionid">>),
	{"Cookie", Cookie};
cookie(bad) ->
	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"baduser">>),
	{"Cookie", Cookie}.

%-define(cookie, begin
%	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"sessionid">>),
%	{"Cookie", Cookie}
%end).
%-define(badcookie, begin
%	Cookie = rpgb_test_util:make_cookie(<<"rpgbsid">>, <<"baduser">>),
%	{"Cookie", Cookie}
%end).
