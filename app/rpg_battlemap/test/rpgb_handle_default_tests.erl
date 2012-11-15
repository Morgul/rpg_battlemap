-module(rpgb_handle_default_tests).

-include_lib("eunit/include/eunit.hrl").

request_test_() ->
	{setup, fun() ->
		rpgb_test_util:web_test_setup(?MODULE)
	end,
	fun(_) ->
		rpgb_test_util:web_test_teardown()
	end,
	fun(_) -> [

		{"Get the favicon", fun() ->
			{ok, Status, _Heads, _Body} = ibrowse:send_req("http://localhost:9091/favicon.ico", [], get),
			?assertEqual("200", Status)
		end},

		{"Get a file that doesn't exist", fun() ->
			{ok, Status, _Heads, _Body} = ibrowse:send_req("http://localhost:9091/alsdhfksfksdahfldsjfljdsalfsafsalj", [], get),
			?assertEqual("404", Status)
		end},

		{"Get bullet.js", fun() ->
			{ok, Status, _Heads, _Body} = ibrowse:send_req("http://localhost:9091/contrib/bullet.js", [], get),
			?assertEqual("200", Status)
		end}

	] end}.
