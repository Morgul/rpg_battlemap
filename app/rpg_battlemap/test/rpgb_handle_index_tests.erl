-module(rpgb_handle_index_tests).

-include_lib("eunit/include/eunit.hrl").
-include("rpg_battlemap.hrl").

request_test_() ->
	{setup, fun() ->
		rpgb_test_util:web_test_setup(?MODULE)
	end,
	fun(_) ->
		rpgb_test_util:web_test_teardown()
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
