-module(web_user, [Id, Name, Openid, GroupId, Permissions, MaxMaps,
	Created, Updated]).

-export([ id/0, name/0, openid/0, group_id/0, group/0, permissions/0,
	max_maps/0, created/0, updated/0
]).

id() -> Id.
name() -> Name.
openid() -> Openid.
group_id() -> GroupId.
group() ->
	case rpgb_data:get_group_by_id(GroupId) of
		{ok, G} -> G;
		_ -> undefined
	end.
permissions() -> Permissions.
max_maps() -> MaxMaps.
created() -> Created.
updated() -> updated.
