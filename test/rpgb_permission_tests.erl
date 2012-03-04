-module(rpgb_permission_tests).
-include_lib("eunit/include/eunit.hrl").
-include("log.hrl").

% TODO boss mock backend.  Fix it to use that.
crud_test_() -> 
	rpgb_util:start_testnode(),
	mnesia:start(),
	mnesia:create_table(rpgb_permission, [{attributes, 
		[id, name, foreign_id]
	}]),
	{foreach, fun() ->
		mnesia:clear_table(rpgb_permission),
		meck:new(needs_permission),
		meck:expect(needs_permission, id, fun() ->
			"needs_permission-1337"
		end)
	end,
	fun(_) ->
		meck:unload(needs_permission),
		ok
	end, [
		fun(_) -> {"granting permission", fun() ->
			Perm = rpgb_permission:new(id, "needs_permission", "create_things"),
			{ok, Perm0} = Perm:save(),
			?debug("ping3"),
			?assertEqual("create_things-needs_permission-1337", Perm0:id()),
			?debug("ping4")
		end} end,

		{"perpetual failure", ?_assert(false)}

	]}.
