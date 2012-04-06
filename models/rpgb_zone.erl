-module(rpgb_zone, [Id, Name :: binary(), BattleId :: atom(),
	StartCellX :: pos_integer(), StartCellY :: pos_integer(),
	Layer :: binary(), ZIndex :: pos_integer(), Rotation :: binary(),
	StrokeOpactiy :: float(), StrokeColor :: binary(), Path :: binary(),
	CreatedTime :: timestamp(), UpdatedTime :: timestamp()]).
-belongs_to(battle).
-compile([export_all]).

before_create() ->
	{ok, ?MODULE:new(Id, Name, BattleId, StartCellX, StartCellY,
	Layer, ZIndex, Rotation, StrokeOpactiy, StrokeColor, Path, erlang:now(),
	erlang:now())}.

before_update() ->
	{ok, ?MODULE:new(Id, Name, BattleId, StartCellX, StartCellY,
	Layer, ZIndex, Rotation, StrokeOpactiy, StrokeColor, Path, CreatedTime,
	erlang:now())}.
