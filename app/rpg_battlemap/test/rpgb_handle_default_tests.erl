-module(rpgb_handle_default_tests).

-include_lib("eunit/include/eunit.hrl").

request_test_() ->
	{setup, fun() ->
		?debugMsg("bing"),
		application:start(cowboy),
		cowboy:start_listener(handle_default_tests, 1,
			cowboy_tcp_transport, [{port, 9091}],
			cowboy_http_protocol, [{dispatch, [{[], rpgb_handle_default, {"localhost", 9091}}]}]
		),
		?debugMsg("bing"),
		ibrowse:start(),
		?debugMsg("bing"),
		rpgb_test_util:mecked_data(handle_default_data)
	end,
	fun(_) ->
		meck:unload(rpgb_data),
		cowboy:stop_listener(handle_default_tests)
	end,
	fun(_) -> [

		

	] end}.
