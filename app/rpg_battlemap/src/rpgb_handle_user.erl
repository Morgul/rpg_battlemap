-module(rpgb_handle_user).

-export([get_routes/0]).

get_routes() ->
	[
			[<<"user">>],
			[<<"user">>, userid]
	].
