-module(rpgb_handle_character).

-export([get_routes/0]).

get_routes() ->
	[
		[<<"character">>],
		[<<"character">>, characterid]
	].
