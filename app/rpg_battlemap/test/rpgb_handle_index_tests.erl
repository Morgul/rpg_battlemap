-module(rpgb_handle_index_tests).

-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

request_test_() ->
	{setup, fun() ->
		application:start(cowboy),
		HostPort = {<<"localhost">>, 9094},
		Routes = rpgb:get_routes(HostPort, [rpgb_handle_index]),
		meck:new(openid),
		cowboy:start_listener(handle_template_tests, 1,
			cowboy_tcp_transport, [{port, 9094}],
			cowboy_http_protocol, [{dispatch, [
				{'_', Routes}
			]}]
		),
		ibrowse:start(),
		rpgb_test_util:mecked_data(handle_template_data)
	end,
	fun(_) ->
		meck:unload(rpgb_data),
		cowboy:stop_listener(handle_template_tests)
	end,
	fun(_) -> [

		{"access without login", fun() ->
			rpgb_session:make_ets(),
			?debugFmt("The sessions:  ~p", [ets:match(rpgb_session, '$1')]),
			{ok, "200", _Heads, Body} = ibrowse:send_req("http://localhost:9094/", [], get),
			{ok, Regex} = re:compile("Sign in"),
			?assertMatch({match, [{_,_}]}, re:run(Body, Regex))
		end},

		{"access while logged in", fun() ->
			rpgb_session:make_ets(),
			{ok, "200", Heads, _Body} = ibrowse:send_req("http://localhost:9094/", [], get),
			Cookie = proplists:get_value("Set-Cookie", Heads),
			Key = ets:first(rpgb_session),
			[Session] = ets:lookup(rpgb_session, Key),
			User = #rpgb_rec_user{
				email = <<"batman@jla.org">>,
				name = <<"Batman">>
			},
			rpgb_session:set_user(User, Session),
			?debugFmt("The sessions:  ~p", [ets:match(rpgb_session, '$1')]),
			{ok, "200", _Heads, Body} = ibrowse:send_req("http://localhost:9094/", [{"Cookie", Cookie}], get),
			?debugFmt("The body:  ~p", [Body]),
			{ok, Regex} = re:compile("Sign Out"),
			?assertMatch({match, [{_,_}]}, re:run(Body, Regex))
		end}

	] end}.
