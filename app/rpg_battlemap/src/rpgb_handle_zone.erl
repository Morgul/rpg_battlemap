-module(rpgb_handle_zone).

-export([get_routes/0]).

get_routes() ->
	[
		[<<"map">>, mapid, <<"layers">>, layerid, <<"auras">>],
		[<<"map">>, mapid, <<"layers">>, layerid, <<"auras">>, zoneid],
		[<<"map">>, mapid, <<"layers">>, layerid, <<"zoness">>],
		[<<"map">>, mapid, <<"layers">>, layerid, <<"zones">>, zoneid]
	].
