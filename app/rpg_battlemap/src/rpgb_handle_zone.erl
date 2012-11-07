-module(rpgb_handle_zone).

-export([get_routes/0]).

get_routes() ->
	[
		[<<"zone">>],
		[<<"zone">>, zoneid]
	].
