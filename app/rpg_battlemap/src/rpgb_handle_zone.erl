-module(rpgb_handle_zone).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, resource_exists/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2]).

-record(ctx, {hostport, session, map, layer, rec, mode}).

get_routes() ->
	[
		<<"/map/:mapid/layers/:layerid/auras">>,
		<<"/map/:mapid/layers/:layerid/auras/:zoneid">>,
		<<"/map/:mapid/layers/:layerid/zones">>,
		<<"/map/:mapid/layers/:layerid/zones/:zoneid">>
	].

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [HostPort]) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{Path, Req2} = cowboy_req:path(Req1),
	{MapId, Req3} = cowboy_req:binding(mapid, Req2),
	Map = try list_to_integer(binary_to_list(MapId)) of
		MapN ->
			case rpgb_data:get_by_id(rpgb_rec_battlemap, MapN) of
				{ok, MapRec} -> MapRec;
				{error, notfound} -> notfound
			end
	catch
		'ERROR':{badarg, _} ->
			notfound
	end,
	{LayerId, Req4} = cowboy_req:binding(layerid, Req3),
	Layer = try list_to_integer(binary_to_list(LayerId)) of
		LayerN ->
			case rpgb_data:get_by_id(rpgb_rec_layer, LayerN) of
				{ok, LayerRec} -> LayerRec;
				{error, notfound} -> notfound
			end
	catch
		'ERROR':{badarg, _} ->
			notfound
	end,
	PreBin = <<"/map/", MapId/binary, "/layers/", LayerId/binary, "/">>,
	PreBinSize = size(PreBin),
	<<PreBin:PreBinSize/binary, RestPath/binary>> = Path,
	Mode2 = case RestPath of
		<<"zones", _/binary>> -> zone;
		<<"auras", _/binary>> -> aura;
		_ -> error
	end,
	{ZoneAuraId, Req5} = cowboy_req:binding(zoneid, Req4),
	ZoneAuraId1 = case ZoneAuraId of
		undefined ->
			undefined;
		_->
			try list_to_integer(binary_to_list(ZoneAuraId)) of
				ZoneAuraIdN ->
					ZoneAuraIdN
			catch
				'ERROR':{badarg,_} ->
					notfound
		end
	end,
	ZoneAura = case {ZoneAuraId1, Mode2} of
		{_, error} ->
			notfound;
		{undefined, _} ->
			undefined;
		{notfound, _} ->
			notfound;
		_ ->
			case rpgb_data:search(rpgb_rec_zone, [{id, ZoneAuraId1},{type, Mode2}]) of
				{ok, []} ->
					notfound;
				{ok, [Rec | _]} ->
					Rec
			end
	end,
	{ok, Req5, #ctx{hostport = HostPort, session = Session, map = Map, layer = Layer, rec = ZoneAura, mode = Mode2}}.

allowed_methods(Req, #ctx{rec = Id} = Ctx) when is_atom(Id) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"DELETE">>], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"post">>}, Req, Ctx};
		_User ->
			{true, Req, Ctx}
	end.

forbidden(Req, #ctx{map = notfound} = Ctx) ->
	{ok, Req2} = cowboy_req:reply(404, Req),
	{halt, Req2, Ctx};
forbidden(Req, #ctx{map = Map, session = Session, mode = zone} = Ctx) ->
	User = rpgb_session:get_user(Session),
	Allowed = case cowboy_req:method(Req) of
		{GetOrHead, Req2} when GetOrHead =:= <<"GET">> orelse GetOrHead =:= <<"HEAD">> ->
			User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id orelse
				lists:member(User#rpgb_rec_user.id, Map#rpgb_rec_battlemap.participant_ids);
		{_Method, Req2} ->
			User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id
	end,
	{not Allowed, Req2, Ctx};
forbidden(Req, #ctx{map = Map, session = Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	Allowed = User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id orelse
		lists:member(User#rpgb_rec_user.id, Map#rpgb_rec_battlemap.participant_ids),
	{not Allowed, Req, Ctx}.

resource_exists(Req, #ctx{map = notfound} = Ctx) ->
	{false, Req, Ctx};
resource_exists(Req, #ctx{layer = notfound} = Ctx) ->
	{false, Req, Ctx};
resource_exists(Req, #ctx{rec = notfound} = Ctx) ->
	{false, Req, Ctx};
resource_exists(Req, #ctx{rec = undefined} = Ctx) ->
	case cowboy_req:method(Req) of
		{'PUT', Req2} ->
			{false, Req2, Ctx};
		{_, Req2} ->
			{true, Req2, Ctx}
	end;
resource_exists(Req, Ctx) ->
	{true, Req, Ctx}.

delete_resource(Req, #ctx{rec = undefined} = Ctx) ->
	{false, Req, Ctx};
delete_resource(Req, Ctx) ->
	Rec = Ctx#ctx.rec,
	rpgb_data:delete(Rec),
	delete_zone(Ctx#ctx.layer, Rec),
	{true, Req, Ctx}.

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

to_json(Req, #ctx{rec = undefined} = Ctx) ->
	Layer = Ctx#ctx.layer,
	Recs = case Ctx#ctx.mode of
		zone ->
			get_zones(Layer#rpgb_rec_layer.first_zone_id);
		aura ->
			get_zones(Layer#rpgb_rec_layer.first_aura_id)
	end,
	Jsons = [make_json(Req, Ctx#ctx{rec = Rec}) || Rec <- Recs],
	{jsx:to_json(Jsons), Req, Ctx};

to_json(Req, #ctx{rec = Rec} = Ctx) ->
	Json = make_json(Req, Ctx),
	{jsx:to_json(Json), Req, Ctx}.

from_json(Req, #ctx{rec = undefined} = Ctx) ->
	#ctx{map = Map, layer = Layer, mode = Mode} = Ctx,
	InitialZone = #rpgb_rec_zone{
		id = undefined, name = <<>>, layer_id = Layer#rpgb_rec_layer.id,
		type = Mode, created = os:timestamp(), updated = os:timestamp()
	},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case validate_zone(Term, InitialZone) of
		{ok, {_Json, Rec}} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			{Layer2, Rec3} = insert_zone(Layer, Rec2),
			Ctx2 = Ctx#ctx{layer = Layer2, rec = Rec3},
			Location = make_location(Req1, Ctx2),
			Req2 = cowboy_req:set_resp_header(<<"location">>, Location, Req1),
			{OutBody, Req3, Ctx3} = to_json(Req2, Ctx2),
			Req4 = cowboy_req:set_resp_body(OutBody, Req3),
			{true, Req4, Ctx3};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			Req3 = cowboy_req:reply(Status, Req2),
			{halt, Req3, Ctx}
	end;
from_json(Req, Ctx) ->
	#ctx{map = Map, layer = Layer, mode = Mode, rec = InitRec} = Ctx,
	InitialZone = InitRec#rpgb_rec_zone{updated = os:timestamp()},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case validate_zone(Term, InitialZone) of
		{ok, {_Json, Rec}} ->
			{ok, Layer2} = delete_zone(Layer, InitialZone),
			{ok, Rec2} = rpgb_data:save(Rec),
			{Layer3, Rec3} = insert_zone(Layer2, Rec2),
			Ctx2 = Ctx#ctx{layer = Layer3, rec = Rec3},
			{OutBody, Req3, Ctx3} = to_json(Req, Ctx2),
			Req4 = cowboy_req:set_resp_body(OutBody, Req3),
			{true, Req4, Ctx3};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			Req3 = cowboy_req:reply(Status, Req2),
			{halt, Req3, Ctx}
	end.

generate_etag(Req, #ctx{map = notfound} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{layer = notfound} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{rec = Atom} = Ctx) when is_atom(Atom) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{rec = Rec} = Ctx) ->
	Bin = jsx:to_json(Rec:to_json()),
	Updated = Rec#rpgb_rec_zone.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.

make_location(Req, Ctx) ->
	#ctx{hostport = {Host, Port}, map = Map, layer = Layer, rec = Rec, mode = Mode} = Ctx,
	ModeList = atom_to_list(Mode),
	rpgb:get_url(Req, Host, Port, ["map", integer_to_list(Map#rpgb_rec_battlemap.id),
		"layers", integer_to_list(Layer#rpgb_rec_layer.id), ModeList ++ "s",
		integer_to_list(Rec#rpgb_rec_zone.id)]).

validate_zone(Json, InitialZone) ->
	ValidateFuns = [
		fun rpgb_validation:scrub_disallowed/1,
		fun scrub_disallowed/1,
		fun rpgb_validation:check_blank_name/1,
		fun check_name_conflict/1,
		fun validate_json_warnings/1,
		fun check_named_zone/1,
		fun check_next_zone_id/1
	],
	rpgb:bind({Json, InitialZone}, ValidateFuns).

scrub_disallowed({[{}], _Zone} = In) ->
	{ok, In};
scrub_disallowed({Json, Zone}) ->
	Disallowed = [<<"type">>],
	Json2 = [KV || {Key, _Value} = KV <- Json, not lists:member(Key, Disallowed)],
	{ok, {Json2, Zone}}.

check_name_conflict({Json, Zone} = In) ->
	ZoneName = #rpgb_rec_zone.name,
	case proplists:get_value(<<"name">>, Json) of
		undefined ->
			{ok, In};
		ZoneName when is_binary(ZoneName) ->
			{ok, In};
		Name ->
			#rpgb_rec_zone{type = Mode, layer_id = LayerId} = Zone,
			case rpgb_data:search(rpgb_rec_zone, [{type, Mode}, {layer_id, LayerId}, {name, Name}]) of
				{ok, []} ->
					{ok, In};
				_ ->
					{error, 409, <<"Name is already used on that layer">>}
			end
	end.

validate_json_warnings({Json, Zone}) ->
	case Zone:from_json(Json, [null_is_undefined]) of
		{ok, Zone2} ->
			{ok, {Json, Zone2}};
		{ok, Zone2, Warnings} ->
			case validate_warnings(Zone2, Warnings) of
				{ok, Zone3} ->
					{ok, {Json, Zone3}};
				Warnings2 ->
					{error, 422, iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Warnings2]))}
			end;
		{_, Else} ->
			{error, 422, iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Else]))}
	end.

validate_warnings(Zone, Warnings) ->
	validate_warnings(Zone, Warnings, []).

validate_warnings(Zone, [], []) ->
	{ok, Zone};
validate_warnings(Zone, [], Acc) ->
	lists:reverse(Acc);
validate_warnings(Zone, [fill_color | Tail], Acc) ->
	case rpgb_validation:is_valid_color(Zone#rpgb_rec_zone.fill_color) of
		true ->
			validate_warnings(Zone, Tail, Acc);
		false ->
			validate_warnings(Zone, Tail, [fill_color | Acc])
	end;
validate_warnings(Zone, [stroke_color | Tail], Acc) ->
	case rpgb_validation:is_valid_color(Zone#rpgb_rec_zone.stroke_color) of
		true ->
			validate_warnings(Zone, Tail, Acc);
		false ->
			validate_warnings(Zone, Tail, [stroke_color| Acc])
	end.

check_named_zone({Json, #rpgb_rec_zone{name = undefined}}) ->
	{error, 422, <<"name cannot be blank">>};
check_named_zone(In) ->
	{ok, In}.

check_next_zone_id({_Json, #rpgb_rec_zone{next_zone_id = undefined}} = In) ->
	{ok, In};
check_next_zone_id({_Json, #rpgb_rec_zone{id = In, next_zone_id = In}}) ->
	{error, 422, <<"zone cannot point to itself as the next in list">>};
check_next_zone_id({_Json, Zone} = In) ->
	#rpgb_rec_zone{next_zone_id = NextId, type = Mode, layer_id = Layer} = Zone,
	case rpgb_data:search(rpgb_rec_zone, [{id, NextId}, {type, Mode}, {layer_id, Layer}]) of
		{ok, []} ->
			{error, 422, <<"Next zone indicated doesn't exist">>};
		{ok, _} ->
			{ok, In}
	end.

insert_zone(#rpgb_rec_layer{first_zone_id = undefined} = Layer, #rpgb_rec_zone{type = zone} = Zone) ->
	Layer2 = Layer#rpgb_rec_layer{first_zone_id = Zone#rpgb_rec_zone.id},
	{ok, Layer3} = rpgb_data:save(Layer2),
	{Layer3, Zone};
insert_zone(#rpgb_rec_layer{first_aura_id = undefined} = Layer, #rpgb_rec_zone{type = aura} = Zone) ->
	Layer2 = Layer#rpgb_rec_layer{first_aura_id = Zone#rpgb_rec_zone.id},
	{ok, Layer3} = rpgb_data:save(Layer2),
	{Layer3, Zone};
insert_zone(#rpgb_rec_layer{first_zone_id = FirstId} = Layer, #rpgb_rec_zone{type = zone, next_zone_id = FirstId} = Zone) ->
	insert_zone(Layer#rpgb_rec_layer{first_zone_id = undefined}, Zone);
insert_zone(#rpgb_rec_layer{first_aura_id = FirstId} = Layer, #rpgb_rec_zone{type = aura, next_zone_id = FirstId} = Zone) ->
	insert_zone(Layer#rpgb_rec_layer{first_aura_id = undefined}, Zone);
insert_zone(Layer, Zone) ->
	#rpgb_rec_zone{type = Type, layer_id = LayerId, next_zone_id = NextId} = Zone,
	{ok, Updates} = rpgb_data:search(rpgb_rec_zone, [{type, Type}, {layer_id, LayerId}, {next_zone_id, NextId}]),
	[Update | _] = [Z || Z <- Updates, Z#rpgb_rec_zone.id =/= Zone#rpgb_rec_zone.id],
	Update2 = Update#rpgb_rec_zone{next_zone_id = Zone#rpgb_rec_zone.id},
	{ok, _} = rpgb_data:save(Update2),
	{Layer, Zone}.

delete_zone(#rpgb_rec_layer{first_aura_id = Id} = Layer, #rpgb_rec_zone{type = aura, id = Id} = Rec) ->
	Layer2 = Layer#rpgb_rec_layer{first_aura_id = Rec#rpgb_rec_zone.next_zone_id},
	rpgb_data:save(Layer2);
delete_zone(#rpgb_rec_layer{first_zone_id = Id} = Layer, #rpgb_rec_zone{type = zone, id = Id} = Rec) ->
	Layer2 = Layer#rpgb_rec_layer{first_zone_id = Rec#rpgb_rec_zone.next_zone_id},
	rpgb_data:save(Layer2);
delete_zone(Layer, Rec) ->
	#rpgb_rec_zone{type = Mode, layer_id = Lid, id = Id, next_zone_id = NextId} = Rec,
	case rpgb_data:search(rpgb_rec_zone, [{type, Mode}, {next_zone_id, Id}, {layer_id, Lid}]) of
		{ok, []} ->
			% huh...
			ok;
		{ok, [Prev | _]} ->
			Prev2 = Prev#rpgb_rec_zone{next_zone_id = NextId},
			rpgb_data:save(Prev2)
	end,
	{ok, Layer}.

make_json(Req, Ctx) ->
	#ctx{hostport = {Host, Port}, rec = Rec, map = Map} = Ctx,
	MapId = Map#rpgb_rec_battlemap.id,
	rpgb_zone:make_json(Req, Host, Port, Rec, MapId).

get_zones(undefined) ->
	[];

get_zones(SeedId) ->
	get_zones(SeedId, []).

get_zones(undefined, Acc) ->
	lists:reverse(Acc);

get_zones(SeedId, Acc) ->
	{ok, Zone} = rpgb_data:get_by_id(rpgb_rec_zone, SeedId),
	NextId = Zone#rpgb_rec_zone.next_zone_id,
	get_zones(NextId, [Zone | Acc]).
