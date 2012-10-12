-module(rpgb_handle_account_tests).

-include_lib("eunit/include/eunit.hrl").

request_test_() ->
	{setup, fun() ->
		application:start(cowboy),
		HostPort = {<<"localhost">>, 9092},
		cowboy:start_listener(handle_account_tests, 1,
			cowboy_tcp_transport, [{port, 9092}],
			cowboy_http_protocol, [{dispatch, [
				{'_', [
					{[<<"account">>], rpgb_handle_account, HostPort},
					{[<<"account">>, <<"login_complete">>], rpgb_handle_account, HostPort}
				]}
			]}]
		),
		ibrowse:start(),
		rpgb_test_util:mecked_data(handle_account_data),
		rpgb_session:make_ets()
	end,
	fun(_) ->
		meck:unload(rpgb_data),
		cowboy:stop_listener(handle_account_tests)
	end,
	fun(_) -> [

		{"access creates new session", fun() ->
			{ok, Status, Heads, _Body} = ibrowse:send_req("http://localhost:9092/account", [], get),
			?assertEqual("401", Status),
			?assertMatch("rpgbsid=" ++ _Cookie, proplists:get_value("Set-Cookie", Heads))
		end},

		{"access doesn't overrite old valid session", fun() ->
			{ok, _Status, Heads, _Body} = ibrowse:send_req("http://localhost:9092/account", [], get),
			Cookie = proplists:get_value("Set-Cookie", Heads),
			{ok, Status, Heads2, _Body} = ibrowse:send_req("http://localhost:9092/account", [{"Cookie", Cookie}], get),
			?assertEqual("401", Status),
			?assertEqual(undefined, proplists:get_value("Set-Cookie", Heads2))
		end},

		{"post kicks off openid", ?_assert(false)},

		{"return from open id creates user", ?_assert(false)},

		{"return from open id get existing user", ?_assert(false)}

	] end}.
