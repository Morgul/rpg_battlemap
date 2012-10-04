-module(rpgb_dets_tests).

-include("rpg_battlemap.hrl").
-include_lib("eunit/include/eunit.hrl").

data_access_test_() ->
	{setup, fun() ->
		file:delete("data"),
		{ok, _} = rpgb_dets:start_link([{data_dir, "."}])
	end,
	fun(_) ->
		rpgb_dets:stop()
	end,
	fun(_) -> [

		{"save", fun() ->
			User = #rpgb_rec_user{name = <<"goober">>},
			?assertMatch({ok, #rpgb_rec_user{id = 1, name = <<"goober">>}}, rpgb_dets:save(User))
		end},

		{"search", fun() ->
			?assertMatch({ok, [#rpgb_rec_user{id = 1, name = <<"goober">>}]}, rpgb_dets:search(rpgb_rec_user, [{name, <<"goober">>}]))
		end},

		{"get by id", fun() ->
			?assertMatch({ok, #rpgb_rec_user{id = 1, name = <<"goober">>}}, rpgb_dets:get_by_id(rpgb_rec_user, 1))
		end},

		{"delete", fun() ->
			?assertEqual({ok, 1}, rpgb_dets:delete(rpgb_rec_user, 1)),
			?assertEqual([], dets:lookup(rpgb_dets, {rpgb_rec_user, 1}))
		end}

	] end}.
