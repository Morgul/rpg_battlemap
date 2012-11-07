-module(rpgb_handle_combatant).

-export([get_routes/0]).

get_routes() ->
	[
		[<<"combatant">>],
		[<<"combatant">>, combatantid]
	].
