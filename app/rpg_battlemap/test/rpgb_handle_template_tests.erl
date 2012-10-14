-module(rpgb_handle_template_tests).

-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

request_test_() ->
	{setup, fun() ->
		application:start(cowboy),
		HostPort = {<<"localhost">>, 9093},
		meck:new(openid),
		cowboy:start_listener(handle_template_tests, 1,
			cowboy_tcp_transport, [{port, 9093}],
			cowboy_http_protocol, [{dispatch, [
				{'_', [
					{[], rpgb_handle_template, {{HostPort}, index}}
				]}
			]}]
		),
		ibrowse:start(),
		rpgb_test_util:mecked_data(handle_template_data),
		rpgb_session:make_ets()
	end,
	fun(_) ->
		meck:unload(rpgb_data),
		cowboy:stop_listener(handle_template_tests)
	end,
	fun(_) -> [

		{"access without login", fun() ->
			{ok, "200", _Heads, Body} = ibrowse:send_req("http://localhost:9093/", [], get),
			{ok, Regex} = re:compile("action=\"http://localhost:9093/account/\""),
			?assertMatch({match, [{_,_}]}, re:run(Body, Regex))
		end},

		{"access while logged in", fun() ->
			{ok, "200", _Heads, Body} = ibrowse:send_req("http://localhost:9093/", [], get),
			{ok, Regex} = re:compile("href=\"/account/logout\""),
			?assertMatch({match, [{_,_}]}, re:run(Body, Regex))
		end}

	] end}.
