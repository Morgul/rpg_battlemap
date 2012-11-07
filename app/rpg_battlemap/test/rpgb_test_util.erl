-module(rpgb_test_util).

-include("rpg_battlemap.hrl").

-export([mecked_data/1]).

mecked_data(Callback) ->
	Ets = ets:new(Callback, [public]),
	ets:insert(Ets, {id_counter, 0}),
	meck:new(rpgb_data),
	meck:expect(rpgb_data, search, fun(Type, Params) ->
		Fields = get_fields(Type),
		Blanked = ['_' || _ <- Fields],
		BlankTuple = list_to_tuple([Type | Blanked]),
		Match = make_match(Params, Fields, BlankTuple),
		case ets:match_object(Ets, {{Type, '_'}, Match}) of
			{error, _} = Out ->
				Out;
			Objects ->
				Objs2 = [Value || {Key, Value} <- Objects],
				{ok, Objs2}
		end
	end),
	meck:expect(rpgb_data, save, fun(Rec) ->
		Type = element(1, Rec),
		{Rec1, Id} = case element(2, Rec) of
			undefined ->
				Id1 = ets:update_counter(Ets, id_counter, 1),
				{setelement(2, Rec, Id1), Id1};
			X ->
				{Rec, X}
		end,
		true = ets:insert(Ets, {{Type, Id}, Rec1}),
		{ok, Rec1}
	end),
	meck:expect(rpgb_data, delete, fun(Rec) ->
		rpgb_data:delete(element(1, Rec), element(2, Rec))
	end),
	meck:expect(rpgb_data, delete, fun(Type, Id) ->
		true = ets:delete(Ets, {Type, Id}),
		{ok, 1}
	end),
	meck:expect(rpgb_data, get_by_id, fun(Type, Id) ->
		Key = {Type, Id},
		case ets:lookup(Ets, Key) of
			[] ->
				{error, notfound};
			[{Key, Rec} | _] ->
				{ok, Rec}
		end
	end),
	meck:expect(rpgb_data, reset, fun(Type, Id) ->
		ets:delete_all_objects(Ets)
	end),
	ok.


get_fields(rpgb_rec_user) -> record_info(fields, rpgb_rec_user);
get_fields(rpgb_rec_user_group) -> record_info(fields, rpgb_rec_user_group);
get_fields(rpgb_rec_battlemap) -> record_info(fields, rpgb_rec_battlemap);
get_fields(rpgb_rec_layer) -> record_info(fields, rpgb_rec_layer);
get_fields(rpgb_rec_zone) -> record_info(fields, rpgb_rec_zone);
get_fields(rpgb_rec_combatant) -> record_info(fields, rpgb_rec_combatant);
get_fields(rpgb_rec_character) -> record_info(fields, rpgb_rec_character).

make_match([], _Fields, Acc) ->
    Acc;
make_match([{Key, Value} | Tail], Fields, Acc) ->
    case listpos(Key, Fields) of
        {error, notfound} ->
            make_match(Tail, Fields, Acc);
        Pos ->
            Acc2 = setelement(Pos + 1, Acc, Value),
            make_match(Tail, Fields, Acc2)
    end.

listpos(Needle, Haystack) ->
    listpos(Needle, Haystack, 1).

listpos(_Needle, [], _Pos) ->
    {error, notfound};
listpos(Needle, [Needle | _Tail], Pos) ->
    Pos;
listpos(Needle, [_NotNeedle | Tail], Pos) ->
    listpos(Needle, Tail, Pos + 1).

