-module(rpgb_data_tests).

-include_lib("eunit/include/eunit.hrl").

data_access_test_() ->
	{setup, fun() ->
		meck:new(data_callback),
		rpgb_data:start_link(data_callback)
	end,
	fun(_) ->
		meck:unload(data_callback),
		DataPid = whereis(rpgb_data),
		unlink(DataPid),
		exit(DataPid, kill)
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

data_event_test_() ->
	{setup, fun() ->
		meck:new(data_callback),
		meck:new(data_event_consumer),
		rpgb_data_events:start_link(),
		rpgb_data:start_link(data_callback)
	end,
	fun(_) ->
		rpgb_data_events:stop(),
		meck:unload(data_callback)
	end,
	fun(_) -> [

		{"install a handler", fun() ->
			Self = self(),
			meck:expect(data_event_consumer, init, fun(hello) ->
				Self ! ok,
				{ok, undefined}
			end),
			rpgb_data_events:subscribe(data_event_consumer, hello),
			Out = receive
				ok ->
					ok
			after 100 ->
				timeout
			end,
			?assertEqual(ok, Out)
		end},

		{"alert on save w/ original undef id means new event", fun() ->
			Self = self(),
			meck:expect(data_event_consumer, handle_event, fun(Msg, _) ->
				Self ! {done, Msg},
				{ok, undefined}
			end),
			meck:expect(data_callback, save, fun({goober, undefined, <<"pants">>}) ->
				{ok, {goober, 1, <<"pants">>}}
			end),
			Rec = {goober, undefined, <<"pants">>},
			{ok, New} = rpgb_data:save(Rec),
			Out = receive
				{done, Msg} ->
					Msg
			after 100 ->
				timeout
			end,
			?assertEqual({new, New}, Out)
		end},

		{"alert on save", fun() ->
			Self = self(),
			meck:expect(data_event_consumer, handle_event, fun(Msg, _) ->
				Self ! {done, Msg},
				{ok, undefined}
			end),
			meck:expect(data_callback, save, fun(In) ->
				{ok, In}
			end),
			Rec = {goober, 3, <<"pants">>},
			{ok, New} = rpgb_data:save(Rec),
			Out = receive
				{done, Msg} ->
					Msg
			after 100 ->
				timeout
			end,
			?assertEqual({update, New}, Out)
		end},

		{"alert on delete", fun() ->
			Self = self(),
			meck:expect(data_event_consumer, handle_event, fun(Msg, _) ->
				Self ! {done, Msg},
				{ok, undefined}
			end),
			meck:expect(data_callback, delete, fun(goober, 4) ->
				{ok, 1}
			end),
			{ok, 1} = rpgb_data:delete({goober, 4, <<"pants">>}),
			Out = receive
				{done, Msg} ->
					Msg
			after 100 ->
				timeout
			end,
			?assertEqual({delete, goober, 4}, Out)
		end}

	] end}.