-module(rpgb_handle_account_tests).

-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

request_test_() ->
	{setup, fun() ->
		application:start(cowboy),
		HostPort = {<<"localhost">>, 9092},
		meck:new(openid),
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

		{"post kicks off openid", fun() ->
			meck:expect(openid, prepare, fun(_SessionId, "http://localhost:9092/openid") ->
				{ok, meck_auth}
			end),
			meck:expect(openid, authentication_url, fun(meck_auth, "http://localhost:9092/account/login_complete", "http://localhost:9092/", Opts) ->
				?assertEqual("nickname", proplists:get_value("openid.sreg.optional", Opts)),
				"http://www.example.com/openid"
			end),
			{ok, _Status, Heads, _Body} = ibrowse:send_req("http://localhost:9092/account", [], get),
			Cookie = proplists:get_value("Set-Cookie", Heads),
			Post = <<"openid=http://localhost:9092/openid">>,
			{ok, Status2, Heads2, _Body} = ibrowse:send_req("http://localhost:9092/account", [{"Cookie", Cookie}], post, Post),
			?assertEqual("303", Status2),
			?assertEqual("http://www.example.com/openid", proplists:get_value("Location", Heads2))
		end},

		{"return from open id creates user", fun() ->
			meck:expect(openid, prepare, fun(_SessionId, "http://localhost:9092/openid") ->
				{ok, meck_auth}
			end),
			meck:expect(openid, authentication_url, fun(meck_auth, "http://localhost:9092/account/login_complete", "http://localhost:9092/", Opts) ->
				?assertEqual("nickname", proplists:get_value("openid.sreg.optional", Opts)),
				"http://www.example.com/openid"
			end),
			meck:expect(openid, verify, fun(_SessionId, "http://localhost:9092/account/login_complete", _QueryString) ->
				{ok, "openid_url_data"}
			end),
			{ok, _Status, Heads, _Body} = ibrowse:send_req("http://localhost:9092/account", [], get),
			Cookie = proplists:get_value("Set-Cookie", Heads),
			Post = <<"openid=http://localhost:9092/openid">>,
			{ok, Status2, Heads2, _Body} = ibrowse:send_req("http://localhost:9092/account", [{"Cookie", Cookie}], post, Post),
			{ok, Status3, Heads3, _Body} = ibrowse:send_req("http://localhost:9092/account/login_complete", [{"Cookie", Cookie}], get, []),
			?assertEqual("303", Status3),
			?assertEqual("/", proplists:get_value("Location", Heads3)),
			?assertMatch({ok, [_Head| _]}, rpgb_data:search(rpgb_rec_user, [{openid, <<"openid_url_data">>}]))
		end},

		{"return from open id get existing user", fun() ->
			Userrec = #rpgb_rec_user{
				openid = <<"openid_url_existant">>,
				name = <<"mega user">>,
				group_id = 1
			},
			{ok, Userrec1} = rpgb_data:save(Userrec),
			meck:expect(openid, prepare, fun(_SessionId, "http://localhost:9092/openid") ->
				{ok, meck_auth}
			end),
			meck:expect(openid, authentication_url, fun(meck_auth, "http://localhost:9092/account/login_complete", "http://localhost:9092/", Opts) ->
				?assertEqual("nickname", proplists:get_value("openid.sreg.optional", Opts)),
				"http://www.example.com/openid"
			end),
			meck:expect(openid, verify, fun(_SessionId, "http://localhost:9092/account/login_complete", _QueryString) ->
				{ok, "openid_url_existant"}
			end),
			{ok, _Status, Heads, _Body} = ibrowse:send_req("http://localhost:9092/account", [], get),
			Cookie = proplists:get_value("Set-Cookie", Heads),
			Post = <<"openid=http://localhost:9092/openid">>,
			{ok, Status2, Heads2, _Body} = ibrowse:send_req("http://localhost:9092/account", [{"Cookie", Cookie}], post, Post),
			{ok, Status3, Heads3, _Body} = ibrowse:send_req("http://localhost:9092/account/login_complete", [{"Cookie", Cookie}], get, []),
			?assertEqual("303", Status3),
			?assertEqual("/", proplists:get_value("Location", Heads3)),
			?assertMatch({ok, [Userrec1]}, rpgb_data:search(rpgb_rec_user, [{openid, <<"openid_url_existant">>}]))



		end}

	] end}.
