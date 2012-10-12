-module(rpgb_handle_default_tests).

-include_lib("eunit/include/eunit.hrl").

request_test_() ->
	{setup, fun() ->
		application:start(cowboy),
		cowboy:start_listener(handle_default_tests, 1,
			cowboy_tcp_transport, [{port, 9091}],
			cowboy_http_protocol, [{dispatch, [
				{'_', [
					{'_', rpgb_handle_default, {<<"localhost">>, 9091}}
				]}
			]}]
		),
		ibrowse:start(),
		rpgb_test_util:mecked_data(handle_default_data)
	end,
	fun(_) ->
		meck:unload(rpgb_data),
		cowboy:stop_listener(handle_default_tests)
	end,
	fun(_) -> [

		{"Get the favicon", fun() ->
			{ok, Status, _Heads, _Body} = ibrowse:send_req("http://localhost:9091/favicon.ico", [], get),
			?assertEqual("200", Status)
		end}

	] end}.
