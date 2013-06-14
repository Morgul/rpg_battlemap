-module(rpgb_prop).

-include_lib("proper/include/proper.hrl").

% higher level structurs
-export([g_mapjson/0, g_combatantjson/0, g_characterjson/0, g_layerjson/0,
	g_zonejson/0]).
% more nuts 'n' bolts
-export([g_name/0, g_color/0, g_opacity/0, g_color_rgb/0,
	g_color_rgba/0, g_256/0, uniquify/1, g_url/0]).

%% higher level

g_mapjson() ->
	?LET(X, list(oneof([
		{name, g_name()},
		{background_color,  g_color()},
		{gridline_color,  g_color()},
		{grid_opacity,  g_opacity()},
		{rating, g_rating()}
	])), uniquify(X)).

g_combatantjson() ->
	?LET(X, list(oneof([
		{name, g_name()},
		{color, g_color()},
		{portrait_image, g_url()},
		{token_image, g_url()},
		{x, integer()},
		{y, integer()},
		{initiative, float()},
		{size, pos_integer()},
		{aura_size, non_neg_integer()},
		{aura_color, oneof([null, g_color()])}
	])), uniquify(X)).

g_characterjson() ->
	?LET(X, list(oneof([
		{name, g_name()},
		{color, g_color()},
		{portrait_image_url, g_url()},
		{token_image_url, g_url()},
		{size, pos_integer()},
		{public, oneof([true, false])}
	])), uniquify(X)).

g_layerjson() ->
	?LET(X, list(oneof([
		{name, g_name()}
	])), uniquify(X)).

g_zonejson() ->
	?LET({Basic, Element}, begin
		BasicFields = list(oneof([
			{<<"name">>, rpgb_prop:g_name()},
			{<<"rotation">>, float(-360.0, 360.0)},
			{<<"stroke_color">>, rpgb_prop:g_color()},
			{<<"stroke_width">>, choose(0, 10)},
			{<<"stroke_opacity">>, float(0.0, 1.0)},
			{<<"fill_color">>, rpgb_prop:g_color()},
			{<<"fill_opacity">>, float(0.0, 1.0)}
		])),
		ElementFields = g_element_fields(),
		{BasicFields, ElementFields}
	end, begin
		uniquify(Basic ++ Element)
	end).

%% nuts n bolts

g_element_fields() ->
	?LET(Element, oneof(['rect', 'circle', 'ellipse', 'line', 'polyline', 'polygon', 'path']), g_make_element_attrs(Element)).

g_make_element_attrs(rect) ->
	[{<<"element_type">>, <<"rect">>},
	{<<"element_attrs">>, [
		{<<"x">>, choose(-100, 100)},
		{<<"y">>, choose(-100, 100)},
		{<<"width">>, choose(1, 100)},
		{<<"height">>, choose(1, 100)}
	]}];
g_make_element_attrs(circle) ->
	[{<<"element_type">>, <<"circle">>},
	{<<"element_attrs">>, [
		{<<"cx">>, choose(-100, 100)},
		{<<"cy">>, choose(-100, 100)},
		{<<"r">>, choose(1, 100)}
	]}];
g_make_element_attrs(ellipse) ->
	[{<<"element_type">>, <<"ellipse">>},
	{<<"element_attrs">>, [
		{<<"cx">>, choose(-100, 100)},
		{<<"cy">>, choose(-100, 100)},
		{<<"rx">>, choose(1, 100)},
		{<<"ry">>, choose(1, 100)}
	]}];
g_make_element_attrs(line) ->
	[{<<"element_type">>, <<"line">>},
	{<<"element_attrs">>, [
		{<<"x1">>, choose(-100, 100)},
		{<<"y1">>, choose(-100, 100)},
		{<<"x2">>, choose(-100, 100)},
		{<<"y2">>, choose(-100, 100)}
	]}];
g_make_element_attrs(polyline) ->
	[{<<"element_type">>, <<"polyline">>},
	{<<"element_attrs">>, [
		{<<"points">>, g_point_list()}
	]}];
g_make_element_attrs(polygon) ->
	[{<<"element_type">>, <<"polygon">>},
	{<<"element_attrs">>, [
		{<<"points">>, g_point_list()}
	]}];
g_make_element_attrs(path) ->
	[{<<"element_type">>, <<"path">>},
	{<<"element_attrs">>, [
		{<<"d">>, g_path()}
	]}].

g_point_list() ->
	?LET({Point1, Points},
		{g_point(), list(g_point())},
		begin
			list_to_binary([integer_to_list(X) ++ [$,] ++ integer_to_list(Y) ++ " " || {X,Y} <- [Point1 | Points]])
		end).

g_path() ->
	?LET(PathSegments, list(g_path_segment()),
	list_to_binary(string:join(PathSegments, " "))).

g_path_segment() ->
	?LET(Type, oneof("MmZzLlHhVvCcSsQqTtAa"), g_path_segment_data(Type)).

g_path_segment_data(ML) when ML =:= $m; ML =:= $M; ML =:= $l; ML =:= $L; ML =:= $t; ML =:= $T ->
	?LET(Points,
		list(g_point()),
		begin
			lists:flatten([ML, $ ] ++ [integer_to_list(X) ++ " " ++ integer_to_list(Y) || {X,Y} <- Points])
		end);
g_path_segment_data(Z) when Z =:= $z; Z =:= $Z ->
	[Z];
g_path_segment_data(HV) when HV =:= $h; HV =:= $H; HV =:= $v; HV =:= $V ->
	?LET(Moves, list(g_xy()),
		begin
			string:join([[HV]] ++ [integer_to_list(M) || M <- Moves], " ")
		end);
g_path_segment_data(LetterC) when LetterC =:= $c; LetterC =:= $C ->
	?LET(Curves, list({g_point(), g_point(), g_point()}),
		begin
			Curves2 = lists:flatten([[A,B,C,D,E,F] || {{A,B},{C,D},{E,F}} <- Curves]),
			string:join([[LetterC]] ++ [integer_to_list(C) || C <- Curves2], " ")
		end);
g_path_segment_data(S) when S =:= $s; S =:= $S; S =:= $q; S =:= $Q ->
	?LET(Curves, list({g_point(), g_point()}),
		begin
			Curves2 = lists:flatten([[A,B,C,D] || {{A,B},{C,D}} <- Curves]),
			string:join([[S]] ++ [integer_to_list(C) || C <- Curves2], " ")
		end);
g_path_segment_data(A) when A =:= $a; A =:= $A ->
	?LET(Params, {g_xy(), g_xy(), choose(-360, 360), choose(0,1), choose(0,1), g_xy(), g_xy()},
		begin
			List = tuple_to_list(Params),
			List2 = [integer_to_list(N) || N <- List],
			string:join([[A]] ++ List2, " ")
		end).

g_xy() ->
	choose(-100, 100).

g_point() ->
	{g_xy(), g_xy()}.

g_unicode_char() ->
	frequency([{1, 9}, {9, integer(32, 126)}, {5, char()}]).

g_name() ->
	?LET({First, Rest}, {g_unicode_char(), list(g_unicode_char())},
		case unicode:characters_to_binary([First | Rest]) of
			{error, Out, _} -> Out;
			Out -> Out
		end).

g_url() ->
	?LET({Proto, Domain, Path},
		{
			oneof([<<"http">>, <<"https">>]),
			?SUCHTHAT(X, list(integer(97, 122)), X =/= []),
			?SUCHTHAT(PList, list( ?SUCHTHAT(PListList, list(integer(97, 122)), length(PListList) =< 10) ), length(PList) =< 10)
		},
		begin
			PathBin = list_to_binary(string:join(Path, "/")),
			DomainBin = list_to_binary(Domain),
			<<Proto/binary, "://", DomainBin/binary, ".com/", PathBin/binary>>
		end
	).

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

g_rating() ->
	oneof([<<"g">>, <<"pg">>, <<"r">>, <<"x">>]).

uniquify(X) ->
	uniquify(X, []).

uniquify([], []) ->
	[{}];

uniquify([], Acc) ->
	Acc;

uniquify([{K, V} | Tail], Acc) ->
	Acc1 = orddict:store(K, V, Acc),
	uniquify(Tail, Acc1).

