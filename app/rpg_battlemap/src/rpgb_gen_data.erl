-module(rpgb_gen_data).

-exports([behaviour_info/1]).

behaviour_info(callbacks) -> [

	{get_user_group_by_id, 1},
	{get_user_group_by_name, 1},
	{save_user_group, 1},
	{delete_user_group, 1},

	{get_web_user_by_id, 1},
	{get_web_user_by_openid, 1},
	{get_web_user_by_name, 1},
	{save_web_user, 1},
	{delete_web_user, 1},

	{get_battlemap_by_id, 1},
	{get_battlemaps_by_owner, 1},
	{get_battlemaps_by_particpant, 1},
	{save_battlemap, 1},
	{delete_battlemap, 1},

	{get_zone_by_id, 1},
	{get_zones_by_battlemap, 1},
	{save_zone, 1},
	{delete_zone, 1},

	{get_combatant_by_id, 1},
	{get_combatants_by_owner, 1},
	{get_comtantants_by_battlemap, 1},
	{save_combatant, 1},
	{delete_combatant, 1}

	];
behaviour_info(_) -> undefined.
