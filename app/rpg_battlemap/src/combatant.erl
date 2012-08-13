-module(combatant, [
	Id,
	Name,
	Battlemap_id,
	Owner_id,
	Color,
	Image,
	X,
	Y,
	Initiative,
	Size,
	Aura_size,
	Aura_color,
	Created,
	Updated
]).

-export([
	id/0,
	name/0,
	battlemap_id/0,
	owner_id/0,
	color/0,
	image/0,
	x/0,
	y/0,
	initiative/0,
	size/0,
	aura_size/0,
	aura_color/0,
	created/0,
	updated/0
]).


id() -> Id.
name() -> Name.
battlemap_id() -> Battlemap_id.
owner_id() -> Owner_id.
color() -> Color.
image() -> Image.
x() -> X.
y() -> Y.
initiative() -> Initiative.
size() -> Size.
aura_size() -> Aura_size.
aura_color() -> Aura_color.
created() -> Created.
updated() -> Updated.
