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
		[<<"map">>, mapid, <<"layers">>, layerid, <<"auras">>],
		[<<"map">>, mapid, <<"layers">>, layerid, <<"auras">>, zoneid],
		[<<"map">>, mapid, <<"layers">>, layerid, <<"zones">>],
		[<<"map">>, mapid, <<"layers">>, layerid, <<"zones">>, zoneid]
	].

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, HostPort) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{Path, Req2} = cowboy_http_req:path(Req1),
	{MapId, Req3} = cowboy_http_req:binding(mapid, Req2),
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
	{LayerId, Req4} = cowboy_http_req:binding(layerid, Req3),
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
	[_Maps, _MapId, _Layers, _LayerId, Mode | _] = Path,
	Mode2 = case Mode of
		<<"zones">> -> zone;
		<<"auras">> -> aura;
		_ -> error
	end,
	{ZoneAuraId, Req5} = cowboy_http_req:binding(zoneid, Req4),
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
	{['GET', 'PUT', 'HEAD'], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{['GET', 'PUT', 'HEAD', 'DELETE'], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"post">>}, Req, Ctx};
		_User ->
			{true, Req, Ctx}
	end.

forbidden(Req, #ctx{map = notfound} = Ctx) ->
	{ok, Req2} = cowboy_http_req:reply(404, Req),
	{halt, Req2, Ctx};
forbidden(Req, #ctx{map = Map, session = Session, mode = zone} = Ctx) ->
	User = rpgb_session:get_user(Session),
	Allowed = User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id,
	{not Allowed, Req, Ctx};
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
	case cowboy_http_req:method(Req) of
		{'PUT', Req2} ->
			{false, Req2, Ctx};
		{_, Req2, Ctx} ->
			{true, Req2, Ctx}
	end;
resource_exists(Req, Ctx) ->
	{true, Req, Ctx}.

delete_resource(Req, Ctx) ->
	{false, Req, Ctx}.

content_types_provided(Req, Ctx) ->
	Types = [
		{{<<"applicatoin">>, <<"json">>, []}, to_json}
	],
	{Types, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, []}, from_json}
	],
	{Types, Req, Ctx}.

to_json(Req, Ctx) ->
	{<<"nope">>, Req, Ctx}.

from_json(Req, Ctx) ->
	{false, Req, Ctx}.

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
