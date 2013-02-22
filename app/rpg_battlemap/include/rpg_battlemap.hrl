% Define the data structures used throughout the app.

-type(time() :: {pos_integer(), non_neg_integer(), non_neg_integer()} | 'undefined').
-type(keyval() :: {binary(), any()}).

-record(rpgb_rec_user, {
	id :: 'undefined' | pos_integer(),
	email :: binary(),
	name :: binary(),
	group_id = 1 :: pos_integer(),
	permissions = [] :: [atom()],
	max_maps = 10 :: pos_integer() | 'infinity',
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_user_group, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	permissions = [] :: [atom()],
	created :: time(),
	updated :: time()
}).
	
-record(rpgb_rec_battlemap, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	owner_id :: pos_integer(),
	participant_ids = [] :: [pos_integer()],
	rating = g :: g | pg | r | x,
	zoom  = 1 :: float(),
	translate_x = 0 :: integer(),
	translate_y = 0 :: integer(),
	grid_spacing = 32 :: pos_integer(),
	background_color = <<"gray">> :: binary(),
	gridline_color = <<"black">> :: binary(),
	grid_opacity = 0.5 :: float(),
	bottom_layer_id :: pos_integer(),
	first_combatant_id :: pos_integer(),
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_layer, {
  id :: 'undefined' | pos_integer(),
  name :: binary(),
  battlemap_id :: pos_integer(),
  next_layer_id :: pos_integer(),
  first_zone_id :: pos_integer(),
  first_aura_id :: pos_integer(),
  created :: time(),
  updated :: time()
}).

-record(rpgb_rec_zone, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	type = 'zone' :: 'zone' | 'aura',
	layer_id :: pos_integer(),
	next_zone_id :: pos_integer(),
	rotation :: float(),
	stroke_color = <<"black">> :: binary(),
	stroke_width = 5 :: non_neg_integer(),
	stroke_opacity = 1 :: float(),
	fill_color = <<"green">> :: binary(),
	fill_opacity = 1,
	element_type = 'rect' :: 'rect' | 'circle' | 'ellipse' | 'line' | 'polyline' | 'polygon' | 'path',
	element_attrs = [] :: [keyval()],
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_combatant, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	battlemap_id :: pos_integer(),
	owner_id :: pos_integer(),
	invisible = false :: boolean(),
	dead = false :: boolean(),
	color = <<"green">> :: binary(),
  portrait_image :: 'undefined' | binary(),
	token_image :: 'undefined' | binary(),
	x = 0 :: integer(),
	y = 0 :: integer(),
  layer_id :: 'undefined' | pos_integer(),
	initiative = 1 :: number(),
	next_combatant_id :: pos_integer(),
	size = 1 :: pos_integer(),
	aura_size = 0 :: non_neg_integer(),
	aura_color :: 'undefined' | binary(),
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_character, {
  id :: 'undefined' | pos_integer(),
  owner_id :: pos_integer(),
	name :: binary(),
  color = <<"green">> :: binary(),
  portrait_image_url :: 'undefined' | binary(),
  token_image_url :: 'undefined' | binary(),
  size = 1 :: pos_integer(),
  public = false :: boolean(),
	created :: time(),
	updated :: time()
}).
