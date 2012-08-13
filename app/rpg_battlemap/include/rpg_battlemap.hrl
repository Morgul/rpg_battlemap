% Define the data structures used throughout the app.

-type(time() :: {pos_integer(), non_neg_integer(), non_neg_integer()} | 'undefined').

-record(user_group, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	permissions = [] :: [atom()],
	created :: time(),
	updated :: time()
}).

-record(user, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	openid :: binary(),
	group_id = 1 :: pos_integer(),
	permissions = [] :: [atom()],
	max_maps = 10 :: pos_integer() | 'infinity',
	created :: time(),
	updated :: time()
}).
	
-record(battlemap, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	owner_id :: pos_integer(),
	participant_ids = [] :: [pos_integer()],
	zoom  = 1 :: float(),
	translate_x = 0 :: integer(),
	translate_y = 0 :: integer(),
	grid_spacing = 32 :: pos_integer(),
	background_color = <<"gray">> :: binary(),
	gridline_color = <<"black">> :: binary(),
	grid_opacity = 0.5 :: float(),
	zones = [] :: [pos_integer()],
	combatants = [] :: [pos_integer()],
	created :: time(),
	updated :: time()
}).

-record(zone, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	battlemap_id :: pos_integer(),
	layer = ground :: 'sky' | 'ground',
	z_index = 1 :: pos_integer(),
	rotation = 'none' :: 'none' | 'ccw' | 'cw' | 'about',
	stroke_width = 5 :: non_neg_integer(),
	stroke_opacity = 1 :: 'float',
	stoke_color = <<"black">> :: binary(),
	fill_color = <<"green">> :: binary(),
	stroke_mode = gappy :: 'gappy' | 'solid',
	path = <<>> :: binary(),
	created :: time(),
	updated :: time()
}).

-record(combatant, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	battlemap_id :: pos_integer(),
	owner_id :: pos_integer(),
	color = <<"green">> :: binary(),
	image :: 'undefined' | binary(),
	x = 0 :: integer(),
	y = 0 :: integer(),
	initiative = 1 :: float(),
	size = 1 :: pos_integer(),
	aura_size = 0 :: non_neg_integer(),
	aura_color :: 'undefined' | binary(),
	created :: time(),
	updated :: time()
}).
