-module(zone, [
	Id,
	Name,
	Battlemap_id,
	Layer,
	Z_index,
	Rotation,
	Stroke_width,
	Stroke_opacity,
	Stoke_color,
	Fill_color,
	Stroke_mode,
	Path,
	Created,
	Updated
]).

-export([
	id/0,
	name/0,
	battlemap_id/0,
	layer/0,
	z_index/0,
	rotation/0,
	stroke_width/0,
	stroke_opacity/0,
	stoke_color/0,
	fill_color/0,
	stroke_mode/0,
	path/0,
	created/0,
	updated/0
]).

id() -> Id.
name() -> Name.
battlemap_id() -> Battlemap_id.
layer() -> Layer.
z_index() -> Z_index.
rotation() -> Rotation.
stroke_width() -> Stroke_width.
stroke_opacity() -> Stroke_opacity.
stoke_color() -> Stoke_color.
fill_color() -> Fill_color.
stroke_mode() -> Stroke_mode.
path() -> Path.
created() -> Created.
updated() -> Updated.
