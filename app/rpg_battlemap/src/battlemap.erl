-module(battlemap, [Id, Name, Owner_id, Participant_ids, Zoom, Translate_x,
	Translate_y, Grid_spacing, Background_color, Gridline_color, Grid_opacity,
	Zones, Combatants, Created, Updated
]).

-export([
	id/0,
	name/0,
	owner_id/0,
	participant_ids/0,
	zoom /0,
	translate_x/0,
	translate_y/0,
	grid_spacing/0,
	background_color/0,
	gridline_color/0,
	grid_opacity/0,
	zones/0,
	combatants/0,
	created/0,
	updated/0
]).


id() -> Id.
name() -> Name.
owner_id() -> Owner_id.
participant_ids() -> Participant_ids.
zoom () -> Zoom.
translate_x() -> Translate_x.
translate_y() -> Translate_y.
grid_spacing() -> Grid_spacing.
background_color() -> Background_color.
gridline_color() -> Gridline_color.
grid_opacity() -> Grid_opacity.
zones() -> rpgb_data:get_zones_by_battlemap(Id).
combatants() -> rpgb_data:get_combatants_by_battlemap(Id).
created() -> Created.
updated() -> Updated.
