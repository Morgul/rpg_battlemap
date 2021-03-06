-module(rpgb_handle_combatant).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, resource_exists/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2
]).

-record(ctx, { hostport, session, mapid, map, combatantid, combatant, layerid}).

get_routes() ->
	[
		<<"/maps/:mapid/combatants">>,
		<<"/maps/:mapid/combatants/:combatantid">>,
		<<"/maps/:mapid/layers/:layerid/combatants">>
	].

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [HostPort]) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{Path, Req2} = cowboy_req:path(Req1),
	{MapId, Req3} = cowboy_req:binding(mapid, Req2),
	MapId1 = case MapId of
		undefined ->
			undefined;
		_ ->
			try list_to_integer(binary_to_list(MapId)) of
				MapN ->
					MapN
			catch
				'error':badarg ->
					undefined
			end
	end,
	{CombatantId, Req4} = cowboy_req:binding(combatantid, Req3),
	CombatantId1 = case CombatantId of
		undefined ->
			mapcombatants;
		_ ->
			try list_to_integer(binary_to_list(CombatantId)) of
				CombatantN -> CombatantN
			catch
				'error':badarg ->
					undefined
			end
	end,
	{LayerId, Req5} = cowboy_req:binding(layerid, Req4),
	{ok, Req5, #ctx{hostport = HostPort, session = Session, mapid = MapId1, combatantid = CombatantId1, layerid = LayerId}}.

%% ===============================
%% restful steps
%% ===============================

allowed_methods(Req, #ctx{layerid = LayerId} = Ctx) when LayerId =/= undefined ->
	{[<<"GET">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"DELETE">>], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"post">>}, Req, Ctx};
		User ->
			{true, Req, Ctx}
	end.

forbidden(Req, #ctx{mapid = MapId, session = Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	case rpgb_data:get_by_id(rpgb_rec_battlemap, MapId) of
		{error, notfound} ->
			{ok, Req2} = cowboy_req:reply(404, Req),
			{halt, Req2, Ctx};
		{ok, Map} ->
			Out = is_participant_or_owner(User, Map),
			{not Out, Req, Ctx#ctx{map = Map}}
	end.

resource_exists(Req, #ctx{layerid = LayerId} = Ctx) when LayerId =/= undefined ->
	#ctx{mapid = MapId} = Ctx,
	case rpgb_data:search(rpgb_rec_layer, [{battlemap_id, MapId},{id,LayerId}]) of
		{ok, []} ->
			{false, Req, Ctx};
		_ ->
			{true, Req, Ctx}
	end;
resource_exists(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	case cowboy_req:method(Req) of
		{'PUT', Req2} ->
			{false, Req2, Ctx};
		{_, Req2} ->
			{true, Req2, Ctx}
	end;
resource_exists(Req, #ctx{combatantid = CombatantId} = Ctx) ->
	case rpgb_data:get_by_id(rpgb_rec_combatant, CombatantId) of
		{ok, Combatant} ->
			{true, Req, Ctx#ctx{combatant = Combatant}};
		{error, notfound} ->
			{false, Req, Ctx}
	end.

delete_resource(Req, Ctx) ->
	#ctx{session = Session, combatant = Combatant, map = Map} = Ctx,
	User= rpgb_session:get_user(Session),
	if
		User#rpgb_rec_user.id =/= Combatant#rpgb_rec_combatant.owner_id ->
			Req2 = cowboy_req:set_resp_body(<<"only owner can delete">>, Req),
			{ok, Req3} = cowboy_req:reply(403, Req2),
			{halt, Req3, Ctx};
		true ->
			{ok, Map2} = delete_combatant(Combatant, Map),
			{true, Req, Ctx#ctx{map = Map2, combatant = undefined}}
	end.

content_types_provided(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, []}, to_json}
	],
	{Types, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, []}, from_json}
	],
	{Types, Req, Ctx}.

to_json(Req, #ctx{layerid = LayerId} = Ctx) when LayerId =/= undefined ->
	#ctx{map = Map} = Ctx,
	Combatants = get_combatants(Map#rpgb_rec_battlemap.first_combatant_id),
	LayerCombatants = [Combatant || #rpgb_rec_combatant{layer_id = Lid} = Combatant <- Combatants, Lid == LayerId],
	Json = [make_json(Req, Ctx, Combatant) || Combatant <- LayerCombatants],
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	#ctx{map = Map} = Ctx,
	#rpgb_rec_battlemap{first_combatant_id = First} = Map,
	Combatants = get_combatants(First),
	Json = [make_json(Req, Ctx, Combatant) || Combatant <- Combatants],
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{combatant = Combatant} = Ctx) ->
	Json = make_json(Req, Ctx, Combatant),
	{jsx:to_json(Json), Req, Ctx}.

from_json(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	#ctx{session = Session, map = Map, mapid = MapId} = Ctx,
	User = rpgb_session:get_user(Session),
	BaseCombatant = #rpgb_rec_combatant{
		id = undefined, name = <<>>,
		battlemap_id = MapId,
		owner_id = User:id(),
		created = os:timestamp(),
		updated = os:timestamp()
	},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case validate_combatant(Term, BaseCombatant) of
		{ok, {Json, Rec}} ->
			Batch = proplists:get_value(<<"batch">>, Json),
			case Batch of
				undefined ->
					{ok, Rec2} = rpgb_data:save(Rec),
					{Map1, Rec3} = insert_combatant(Map, Rec2),
					Ctx2 = Ctx#ctx{combatant = Rec3, combatantid = Rec3#rpgb_rec_combatant.id, map = Map1},
					Location = make_location(Req1, Ctx2, Rec3),
					Req2 = cowboy_req:set_resp_header(<<"location">>, Location, Req1),
					{OutBody, Req3, Ctx3} = to_json(Req2, Ctx2),
					Req4 = cowboy_req:set_resp_body(OutBody, Req3),
					{true, Req4, Ctx3};
				N when is_integer(N), 1 =< N, N =< 20 ->
					{Map1, Combatants} = make_combatants(N, Rec, Map),
					Jsons = [make_json(Req, Ctx, C) || C <- Combatants],
					Req2 = cowboy_req:set_resp_body(jsx:to_json(Jsons), Req1),
					{true, Req2, Ctx};
				_ ->
					OutBody = io_lib:format("batch must be 1 to 20; you gave me ~p", [Batch]),
					Req2 = cowboy_req:set_resp_body(OutBody, Req),
					{ok, Req3} = cowboy_req:reply(422, Req2),
					{halt, Req3, Ctx}
			end;
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			{ok, Req3} = cowboy_req:reply(Status, Req2),
			{halt, Req3, Ctx}
	end;

from_json(Req, Ctx) ->
	#ctx{session = Session, map = Map, mapid = MapId} = Ctx,
	User = rpgb_session:get_user(Session),
	BaseCombatant = Ctx#ctx.combatant#rpgb_rec_combatant{updated = os:timestamp()},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case validate_combatant(Term, BaseCombatant) of
		{ok, {Json, Rec}} ->
			remove_combatant(Map, BaseCombatant),
			{ok, Rec3} = rpgb_data:save(Rec),
			{ok, Map2} = rpgb_data:get_by_id(rpgb_rec_battlemap, Map#rpgb_rec_battlemap.id),
			{Map3, Rec4} = insert_combatant(Map2, Rec3),
			Ctx2 = Ctx#ctx{combatant = Rec4, map = Map3},
			{OutBody, Req2, Ctx3} = to_json(Req1, Ctx2),
			Req3 = cowboy_req:set_resp_body(OutBody, Req2),
			{true, Req3, Ctx2};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			{ok, Req3} = cowboy_req:reply(Status, Req2),
			{halt, Req3, Ctx}
	end.

generate_etag(Req, #ctx{combatant = undefined} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{combatant = Combatant} = Ctx) ->
	Bin = jsx:to_json(Combatant:to_json()),
	Updated = Combatant#rpgb_rec_combatant.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.

%% ===============================
%% internal functions
%% ===============================

get_combatants(FirstId) ->
	get_combatants(FirstId, []).

get_combatants(undefined, Acc) ->
	lists:reverse(Acc);

get_combatants(Id, Acc) ->
	{ok, C} = rpgb_data:get_by_id(rpgb_rec_combatant, Id),
	get_combatants(C#rpgb_rec_combatant.next_combatant_id, [C | Acc]).

is_participant_or_owner(#rpgb_rec_user{id = Id}, #rpgb_rec_battlemap{owner_id = Id}) ->
	true;
is_participant_or_owner(#rpgb_rec_user{id = Id}, Map) ->
	Partiers = Map#rpgb_rec_battlemap.participant_ids,
	lists:member(Id, Partiers).

delete_combatant(#rpgb_rec_combatant{id = Id} = Combatant, #rpgb_rec_battlemap{first_combatant_id = Id} = Map) ->
	rpgb_data:delete(Combatant),
	NewFirst = Combatant#rpgb_rec_combatant.next_combatant_id,
	Map2 = Map#rpgb_rec_battlemap{first_combatant_id = NewFirst},
	rpgb_data:save(Map2);

delete_combatant(Combatant, Map) ->
	Map2 = remove_combatant(Map, Combatant),
	rpgb_data:delete(Combatant),
	{ok, Map2}.

make_json(Req, Ctx, Combatant) ->
	Url = make_location(Req, Ctx, Combatant),
	Combatant:to_json([{url, Url}, {null_is_undefined}]).

make_location(Req, Ctx, Combatant) ->
	{Host, Port} = Ctx#ctx.hostport,
	rpgb:get_url(Req, Host, Port, ["map", integer_to_list(Combatant#rpgb_rec_combatant.battlemap_id), "combatants", integer_to_list(Combatant#rpgb_rec_combatant.id)]).

validate_combatant(Json, InitRec) ->
	ValidateFuns = [
		fun scrub_disallowed/1,
		fun check_named_combatant/1,
		fun check_blank_name/1,
		fun validate_json/1,
		fun check_next_combatant_id/1,
		fun check_next_combatant_self/1,
		fun check_valid_layer/1
	],
	rpgb:bind({Json, InitRec}, ValidateFuns).

scrub_disallowed({Json, Rec}) ->
	Disallowed = [<<"id">>, <<"battlemap_id">>, <<"owner_id">>, <<"created">>, <<"updated">>],
	Json2 = scrub_disallowed(Json, Disallowed),
	{ok, {Json2, Rec}}.

scrub_disallowed(Json, []) ->
	Json;
scrub_disallowed(Json, [Key | Tail] = Nopes) ->
	case proplists:delete(Key, Json) of
		Json ->
			scrub_disallowed(Json, Tail);
		Json2 ->
			scrub_disallowed(Json2, Nopes)
	end.

check_valid_layer({_Json, #rpgb_rec_combatant{layer_id = undefined}} = In) ->
	{ok, In};
check_valid_layer({Json, Rec} = In) ->
	#rpgb_rec_combatant{layer_id = LayerId, battlemap_id = MapId} = Rec,
	case rpgb_data:search(rpgb_rec_layer, [{id, LayerId}, {battlemap_id, MapId}]) of
		{ok, []} ->
			{error, 422, <<"no such layer">>};
		{ok, Layers} when length(Layers) >= 1 ->
			{ok, In}
	end.

check_named_combatant({Json, Rec} = In) ->
	case {proplists:get_value(<<"name">>, Json), Rec#rpgb_rec_combatant.name} of
		{undefined, undefined} ->
			{error, 422, <<"name cannot be blank">>};
		_ ->
			{ok, In}
	end.

check_blank_name({Json, _Rec} = In) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, 422, <<"name cannot be blank">>};
		_ ->
			{ok, In}
	end.

validate_json({Json, Rec}) ->
	case Rec:from_json(Json, [null_is_undefined]) of
		{ok, Rec2} ->
			{ok, {Json, Rec2}};
		{ok, Rec2, Warnings} ->
			case validate_warnings(Rec2, Warnings) of
				{ok, Rec3} ->
					{ok, {Json, Rec3}};
				{error, Rec3, Warnings2} ->
					Body = iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Warnings2])),
					{error, 422, Body}
			end;
		{_, Else} ->
			Body = iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Else])),
			{error, 422, Body}
	end.

validate_warnings(Rec, Warnings) ->
	validate_warnings(Rec, Warnings, []).

validate_warnings(Rec, [], []) ->
	{ok, Rec};
validate_warnings(Rec, [], StillBad) ->
	{error, Rec, lists:reverse(StillBad)};
validate_warnings(Rec, [aura_color | Tail], Acc) ->
	case is_valid_color(Rec#rpgb_rec_combatant.aura_color) of
		true ->
			validate_warnings(Rec, Tail, Acc);
		false ->
			validate_warnings(Rec, Tail, [aura_color | Acc])
	end;
validate_warnings(Rec, [color | Tail], Acc) ->
	case is_valid_color(Rec#rpgb_rec_combatant.color) of
		true ->
			validate_warnings(Rec, Tail, Acc);
		false ->
			validate_warnings(Rec, Tail, [color | Acc])
	end;
validate_warnings(Rec, [Head | Tail], Acc) ->
	validate_warnings(Rec, Tail, [Head | Acc]).

is_valid_color([_R, _G, _B] = RGB) ->
	lists:all(fun(E) -> E < 256 andalso 0 =< E end, RGB);
is_valid_color([R,B,G,A]) ->
	RGB = [R,G,B],
	AValid = ( 0 =< A andalso A =< 1 ),
	AValid andalso is_valid_color(RGB).

check_next_combatant_self({Json, Rec} = In) ->
	#rpgb_rec_combatant{id = Id} = Rec,
	case proplists:get_value(<<"next_combatant_id">>, Json) of
		Id when Id =/= undefined ->
			{error, 422, <<"combatant cannot point to itself">>};
		_ ->
			{ok, In}
	end.

check_next_combatant_id({Json, Rec} = In) ->
	NextId = proplists:get_value(<<"next_combatant_id">>, Json),
	case NextId of
		null ->
			{ok, {Json, Rec#rpgb_rec_combatant{next_combatant_id = undefined}}};
		undefined ->
			{ok, In};
		_ ->
			#rpgb_rec_combatant{battlemap_id = MapId} = Rec,
			case rpgb_data:search(rpgb_rec_combatant, [{battlemap_id, MapId},{id,NextId}]) of
				{ok, []} ->
					{error, 422, <<"combatant set up as next does not exist">>};
				_ ->
					{ok, {Json, Rec#rpgb_rec_combatant{next_combatant_id = NextId}}}
			end
	end.

make_combatants(N, Rec, Map) ->
	DoneFun = make_done_fun(Rec, Map),
	NextId = Rec#rpgb_rec_combatant.next_combatant_id,
	make_combatants(N, NextId, Rec, DoneFun, []).

make_combatants(N, NextId, _Rec, DoneFun, Acc) when N < 1 ->
	Map2 = DoneFun(NextId),
	{Map2, Acc};

make_combatants(N, NextId, Rec, DoneFun, Acc) ->
	Name = make_name(Rec, N),
	Rec2 = Rec#rpgb_rec_combatant{name = Name, next_combatant_id = NextId},
	{ok, Rec3} = rpgb_data:save(Rec2),
	make_combatants(N - 1, Rec3#rpgb_rec_combatant.id, Rec, DoneFun, [Rec3 | Acc]).

make_done_fun(#rpgb_rec_combatant{next_combatant_id = Id}, #rpgb_rec_battlemap{first_combatant_id = Id} = Map) ->
	fun(NewFirst) ->
		{ok, Map2} = rpgb_data:save(Map#rpgb_rec_battlemap{first_combatant_id = NewFirst}),
		Map2
	end;
make_done_fun(#rpgb_rec_combatant{next_combatant_id = Id}, Map) ->
	{ok, [Prev | _]} = rpgb_data:search(rpgb_rec_combatant, [{next_combatant_id, Id}]),
	fun(NewNext) ->
		{ok, _} = rpgb_data:save(Prev#rpgb_rec_combatant{next_combatant_id = NewNext}),
		Map
	end.

make_name(#rpgb_rec_combatant{name = BaseName}, N) ->
	make_name(BaseName, N);
make_name(BaseName, N) ->
	BinN = list_to_binary(integer_to_list(N)),
	<<BaseName/binary, " ", BinN/binary>>.

remove_combatant(#rpgb_rec_battlemap{first_combatant_id = Id} = Map, #rpgb_rec_combatant{id = Id} = Combatant) ->
	{ok, Map2} = rpgb_data:save(Map#rpgb_rec_battlemap{first_combatant_id = Combatant#rpgb_rec_combatant.next_combatant_id}),
	Map2;

remove_combatant(Map, Combatant) ->
	{ok, [Prev | _]} = rpgb_data:search(rpgb_rec_combatant, [{next_combatant_id, Combatant#rpgb_rec_combatant.id}]),
	rpgb_data:save(Prev#rpgb_rec_combatant{next_combatant_id = Combatant#rpgb_rec_combatant.next_combatant_id}),
	Map.

insert_combatant(_Map, #rpgb_rec_combatant{id = undefined}) ->
	erlang:error(badarg);

insert_combatant(#rpgb_rec_battlemap{first_combatant_id = undefined} = Map, Combatant) ->
	{ok, Map2} = rpgb_data:save(Map#rpgb_rec_battlemap{first_combatant_id = Combatant#rpgb_rec_combatant.id}),
	{Map2, Combatant};

insert_combatant(#rpgb_rec_battlemap{first_combatant_id = Id} = Map, #rpgb_rec_combatant{next_combatant_id = Id} = Combatant) ->
	{ok, Map2} = rpgb_data:save(Map#rpgb_rec_battlemap{first_combatant_id = Combatant#rpgb_rec_combatant.id}),
	{Map2, Combatant};

insert_combatant(Map, Combatant) ->
	#rpgb_rec_combatant{id = Id, next_combatant_id = NextId} = Combatant,
	{ok, Prevs} = rpgb_data:search(rpgb_rec_combatant, [{next_combatant_id, NextId}]),
	case [C || #rpgb_rec_combatant{id = Cid} = C <- Prevs, Cid =/= Id] of
		[] ->
			ok;
		[Prev | _] ->
			{ok, Prev2} = rpgb_data:save(Prev#rpgb_rec_combatant{next_combatant_id = Combatant#rpgb_rec_combatant.id})
	end,
	{Map, Combatant}.
