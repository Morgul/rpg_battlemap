-module(rpgb_validation).

-export([scrub_disallowed/1, check_blank_name/1, is_valid_color/1]).

scrub_disallowed({[{}], _Rec} = In) ->
	{ok, In};
scrub_disallowed({Json, Rec}) ->
	Disallowed = [<<"id">>, <<"created">>, <<"updated">>, <<"owner_id">>,
		<<"battlemap_id">>, <<"layer_id">>],
	Json2 = [KV || {Key, _Value} = KV <- Json, not lists:member(Key, Disallowed)],
	{ok, {Json2, Rec}}.

check_blank_name({[{}], _Rec} = In) ->
	{ok, In};
check_blank_name({Json, Rec} = In) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, 422, <<"Name cannot be blank">>};
		_ ->
			{ok, In}
	end.

is_valid_color([_R, _G, _B] = RGB) ->
	lists:all(fun(E) -> E < 256 andalso 0 =< E end, RGB);
is_valid_color([R,G,B,A]) ->
	RGB = [R,G,B],
	AValid = ( 0 =< A andalso A =< 1 ),
	AValid andalso is_valid_color(RGB).
