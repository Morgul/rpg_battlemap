-module(rpgb_prop).

-include_lib("proper/include/proper.hrl").

-export([g_mapjson/0, g_name/0, g_color/0, g_opacity/0, g_color_rgb/0,
	g_color_rgba/0, g_256/0, uniquify/1]).

g_mapjson() ->
	?LET(X, list(oneof([
		{name, g_name()},
		{background_color,  g_color()},
		{gridline_color,  g_color()},
		{grid_opacity,  g_opacity()}
	])), uniquify(X)).

g_name() ->
	?LET(N,
	list(
		frequency([
			{1, 9},
			{8, integer(32, 126)},
			{5, char()}
		])
	), unicode:characters_to_binary(N)).

g_color() ->
	oneof([
		<<"black">>, <<"blue">>, <<"green">>, g_color_rgb(), g_color_rgba()
	]).

g_opacity() ->
	?LET(N, int(), case N of 0 -> 0.0; _ -> 1 / abs(N) end).

g_color_rgb() ->
	[R,B,G,_] = g_color_rgba(),
	[R,B,G].

g_color_rgba() ->
	[g_256(), g_256(), g_256(), g_opacity()].

g_256() ->
	choose(0, 255).

uniquify(X) ->
	uniquify(X, []).

uniquify([], Acc) ->
	Acc;

uniquify([{K, V} | Tail], Acc) ->
	Acc1 = orddict:store(K, V, Acc),
	uniquify(Tail, Acc1).

