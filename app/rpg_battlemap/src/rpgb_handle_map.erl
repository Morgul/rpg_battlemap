-module(rpgb_handle_map).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, to_html/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2, resource_exists/2]).

-record(ctx, { hostport, session, mapid, map}).

get_routes() ->
	[
		<<"/maps">>,
		<<"/maps/:mapid">>
	].

init(_Protos, _Req, _HostPort) ->
	?info("usual map stuff"),
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [HostPort]) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	%?debug("Session:  ~p", [Session]),
	{_Path, Req2} = cowboy_req:path(Req1),
	%?debug("path:  ~p", [Path]),
	{MapId, Req3} = cowboy_req:binding(mapid, Req2),
	MapId1 = case MapId of
		undefined -> undefined;
		_ ->
			try list_to_integer(binary_to_list(MapId)) of
				N -> N
			catch
				'ERROR':{badarg, _} ->
					<<"bad map id, will 404 later">>
			end
	end,
	{ok, Req3, #ctx{hostport = HostPort, session = Session, mapid = MapId1}}.

allowed_methods(Req, #ctx{mapid = undefined} = Ctx) ->
	{[<<"GET">>, <<"HEAD">>, <<"POST">>], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"DELETE">>], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			rpgb:refresh_templates(base_dtl),
			User = rpgb_session:get_user(Ctx#ctx.session),
			LoginLink = rpgb:get_url(Req, ["account", "login"]),
			LogoutLink = rpgb:get_url(Req, ["account", "logout"]),
			RenderProps = [{user, User}, {login_link, LoginLink},
				{logout_link, LogoutLink},
				{content, <<"You are not logged in!">>}],
			{ok, Output} = base_dtl:render(RenderProps),
			Req1 = cowboy_req:set_resp_body(Output, Req),
			{{false, <<"persona">>}, Req1, Ctx};
		_User ->
			{true, Req, Ctx}
	end.

forbidden(Req, #ctx{mapid = MapId, session = Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	?debug("User in question:  ~p", [User]),
	case MapId of
		undefined ->
			{false, Req, Ctx};
		_ ->
			case rpgb_data:get_by_id(rpgb_rec_battlemap, MapId) of
				{ok, Map} ->
					?debug("map found:  ~p", [Map]),
					IsOwner = User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id,
					IsParticpant = rpgb_rec_battlemap:is_user_participant(User, Map),
					{Method, Req1} = cowboy_req:method(Req),
					IsForbidden = case {IsOwner, IsParticpant, Method} of
						{true, _, _} ->
							false;
						{_, true, <<"GET">>} ->
							false;
						_ ->
							true
					end,
					{IsForbidden, Req1, Ctx#ctx{map = Map}};
				{error, notfound} ->
					{false, Req, Ctx#ctx{map = {error, notfound}}}
			end
	end.

resource_exists(Req, #ctx{mapid = undefined} = Ctx) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req1} ->
			{false, Req1, Ctx};
		{_, Req1} ->
			{true, Req1, Ctx}
	end;

resource_exists(Req, #ctx{mapid = {error, notfound}} = Ctx) ->
	{false, Req, Ctx};

resource_exists(Req, Ctx) ->
	{true, Req, Ctx}.

delete_resource(Req, #ctx{mapid = undefined} = Ctx) ->
	{false, Req, Ctx};

delete_resource(Req, #ctx{mapid = MapId} = Ctx) ->
	case rpgb_data:delete(rpgb_rec_battlemap, MapId) of
		{ok, _} ->
			{true, Req, Ctx};
		{error, Err} ->
			Body = iolist_to_binary(io_lib:format("Error deleting:  ~p", [Err])),
			{ok, Req1} = cowboy_req:set_resp_body(Body),
			{false, Req1, Ctx}
	end.

content_types_provided(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, to_json},
		{{<<"text">>, <<"html">>, '*'}, to_html}
	],
	{Types, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, from_json}
	],
	{Types, Req, Ctx}.

to_json(Req, #ctx{map = undefined} = Ctx) ->
	Session = Ctx#ctx.session,
	User = rpgb_session:get_user(Session),
	{ok, Maps} = rpgb_rec_battlemap:get_by_participant(User),
	Json = lists:map(fun(M) ->
		make_json(Req, Ctx, M)
	end, Maps),
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{map = Map} = Ctx) ->
	Json = make_json(Req, Ctx, Map),
	{jsx:to_json(Json), Req, Ctx}.

to_html(Req, #ctx{map = undefined} = Ctx) ->
	{<<"html">>, Req, Ctx};

to_html(Req, Ctx) ->
	rpgb:refresh_templates(map_dtl),
	{Host, Port} = Ctx#ctx.hostport,
	Json = rpgb_rec_battlemap:make_json(Ctx#ctx.map),
	%Json = make_json(Req, Ctx, Ctx#ctx.map),
	User = rpgb_session:get_user(Ctx#ctx.session),
	LayerUrl = make_location(Req, Ctx, Ctx#ctx.map),
	LayerUrl2 = <<LayerUrl/binary, "/layers">>,
	CombatantUrl = make_location(Req, Ctx, Ctx#ctx.map),
	CombatantUrl2 = <<CombatantUrl/binary, "/combatants">>,
	LoginLink = rpgb:get_url(Req, ["account", "login"]),
	LogoutLink = rpgb:get_url(Req, ["account", "logout"]),
	Json2 = [{combatants_url, CombatantUrl2}, {layers_url, LayerUrl2} | Json],
	PatternHelper = [begin
		[{x, Row * 32}, {y, Col * 32}]
	end || Row <- lists:seq(0, 15), Col <- lists:seq(0, 15)],
	{ok, Output} = map_dtl:render([{user, User}, {map, Json2},
		{pattern_helper, PatternHelper}, {map_json, jsx:to_json(Json2)},
		{login_link, LoginLink}, {logout_link, LogoutLink}]),
	{Output, Req, Ctx}.

from_json(Req, #ctx{mapid = MapId} = Ctx) ->
	#ctx{session = Session} = Ctx,
	User = rpgb_session:get_user(Session),
	InitialMap = case MapId of
		undefined ->
			?debug("creating new map"),
			#rpgb_rec_battlemap{
				id = undefined,
				owner_id = User#rpgb_rec_user.id,
				participant_ids = [],
				bottom_layer_id = undefined,
				first_combatant_id = undefined,
				created = os:timestamp(),
				updated = os:timestamp()
			};
		_ ->
			?debug("updating existing map ~p", [MapId]),
			InitM = Ctx#ctx.map,
			InitM#rpgb_rec_battlemap{updated = os:timestamp()}
	end,
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	?debug("Submitted json:  ~p", [Term]),
	%case validate_map(Term, InitialMap) of
	case rpgb_rec_battlemap:update_from_json(Term, InitialMap) of
		{ok, Rec} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			Location = make_location(Req, Ctx, Rec2),
			{ok, Req2, Rec3} = case MapId of
				undefined ->
					OutReq = cowboy_req:set_resp_header(<<"location">>, Location, Req1),
					LayerRec = #rpgb_rec_layer{battlemap_id = Rec2#rpgb_rec_battlemap.id, name = <<"Bottom Layer">>},
					{ok, LayerRec2} = rpgb_data:save(LayerRec),
					{ok, OutRec} = rpgb_data:save(Rec2#rpgb_rec_battlemap{bottom_layer_id = LayerRec2#rpgb_rec_layer.id}),
					{ok, OutReq, OutRec};
				_ ->
					{ok, Req1, Rec2}
			end,
			OutJson = jsx:to_json(make_json(Req2, Ctx, Rec3)),
			Req3 = cowboy_req:set_resp_body(OutJson, Req2),
			Return = case MapId of
				undefined ->
					{true, make_location(Req3, Ctx, Rec3)};
				_ ->
					true
			end,
			?debug("The return is: ~p", [Return]),
			{Return, Req3, Ctx#ctx{mapid = Rec2#rpgb_rec_battlemap.id, map = Rec3}};
		{error, {Atom, ErrString}} ->
			?debug("error on map thinging: ~p", [{Atom, ErrString}]),
			ErrBody2 = jsx:to_json(ErrString),
			Status = case Atom of
				conflict -> 409;
				invalid -> 422
			end,
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			{ok, Req3} = cowboy_req:reply(Status, Req2),
			{halt, Req3, Ctx};
		Wut ->
			?debug("no idea what's up: ~p", [Wut]),
			{ok, Req2} = cowboy_req:reply(500, Req),
			{halt, Req2, Ctx}
	end.

make_json(_Req, _Ctx, Map) ->
	rpgb_rec_battlemap:make_json(Map).

make_location(Req, Ctx, Rec) ->
	{Host, Port} = Ctx#ctx.hostport,
	rpgb:get_url(Req, Host, Port, ["maps", integer_to_list(Rec#rpgb_rec_battlemap.id)]).

generate_etag(Req, #ctx{mapid = undefined} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{map = Map} = Ctx) ->
	Bin = jsx:to_json(Map:to_json()),
	Updated = Map#rpgb_rec_battlemap.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.
