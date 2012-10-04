-module(rpgb_data_tests).

-include_lib("eunit/include/eunit.hrl").

data_access_test_() ->
	{setup, fun() ->
		meck:new(data_callback),
		rpgb_data:start_link(data_callback)
	end,
	fun(_) ->
		meck:unload(data_callback)
	end,
	fun(_) -> [

		{"get by id, all okay", fun() ->
			meck:expect(data_callback, get_by_id, fun(goober, 5) ->
				{ok, {goober, 5, <<"pants">>}}
			end),
			?assertEqual({ok, {goober, 5, <<"pants">>}}, rpgb_data:get_by_id(goober, 5))
		end},

		{"get by id, not found", fun() ->
			meck:expect(data_callback, get_by_id, fun(goober, 5) ->
				{error, notfound}
			end),
			?assertEqual({error, notfound}, rpgb_data:get_by_id(goober, 5))
		end},

		{"get by id, some other error", fun() ->
			meck:expect(data_callback, get_by_id, fun(goober, 5) ->
				{error, explosions}
			end),
			?assertEqual({error, explosions}, rpgb_data:get_by_id(goober, 5))
		end},

		{"save", fun() ->
			meck:expect(data_callback, save, fun({goober, undefined, <<"pants">>} = Tuple) ->
				{ok, setelement(2, Tuple, 1)}
			end),
			?assertEqual({ok, {goober, 1, <<"pants">>}}, rpgb_data:save({goober, undefined, <<"pants">>}))
		end},

		{"delete arity 2", fun() ->
			meck:expect(data_callback, delete, fun(goober, 3) ->
				{ok, 1}
			end),
			?assertEqual({ok, 1}, rpgb_data:delete(goober, 3))
		end},

		{"delete arity 1 becomes two", fun() ->
			meck:expect(data_callback, delete, fun(goober, 3) ->
				{ok, 1}
			end),
			?assertEqual({ok, 1}, rpgb_data:delete({goober, 3, <<"pants">>}))
		end}

	] end}.
