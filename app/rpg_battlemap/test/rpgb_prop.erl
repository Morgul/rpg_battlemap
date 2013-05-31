-module(rpgb_prop).

-include_lib("proper/include/proper.hrl").

% higher level structurs
-export([g_mapjson/0, g_combatantjson/0, g_characterjson/0]).
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
		{layer_id, choose(1, 3)},
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

%% nuts n bolts

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

