-module(rpgb_handle_layer).

-export([get_routes/0]).

get_routes() ->
	[
		[<<"layer">>],
		[<<"layer">>, layerid]
	].
