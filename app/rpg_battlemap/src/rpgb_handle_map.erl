-module(rpgb_handle_map).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, to_html/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2]).

-record(ctx, { hostport, session, mapid, map}).

get_routes() ->
	[
		<<"/map">>,
		<<"/map/:mapid">>
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
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>], Req, Ctx};

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
			{{false, <<"post">>}, Req1, Ctx};
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
					if
						User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id ->
							{false, Req, Ctx#ctx{map = Map}};
						true ->
							{true, Req, Ctx#ctx{map = Map}}
					end;
				{error, notfound} ->
					rpgb:refresh_templates(base_dtl),
					LoginLink = rpgb:get_url(Req, ["account", "login"]),
					LogoutLink = rpgb:get_url(Req, ["account", "logout"]),
					RenderProps = [{user, User}, {login_link, LoginLink},
						{logout_link, LogoutLink},
						{content, <<"This map doesn't exist!">>}],
					{ok, Output} = base_dtl:render(RenderProps),
					Req2 = cowboy_req:set_resp_body(Output, Req),
					{ok, Req3} = cowboy_req:reply(404, Req2),
					{halt, Req3, Ctx}
			end
	end.

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
		{{<<"application">>, <<"json">>, []}, to_json},
		{{<<"text">>, <<"html">>, []}, to_html}
	],
	{Types, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, []}, from_json}
	],
	{Types, Req, Ctx}.

to_json(Req, #ctx{map = Map} = Ctx) ->
	Json = make_json(Req, Ctx, Map),
	{jsx:to_json(Json), Req, Ctx}.

to_html(Req, #ctx{map = undefined} = Ctx) ->
	{<<"html">>, Req, Ctx};

to_html(Req, Ctx) ->
	rpgb:refresh_templates(map_dtl),
	{Host, Port} = Ctx#ctx.hostport,
	Json = rpgb_rec_battlemap:make_json(Req, Host, Port, Ctx#ctx.map),
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
	case validate_map(Term, InitialMap) of
		{ok, {_DerJson, Rec}} ->
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
			{true, Req3, Ctx#ctx{mapid = Rec2#rpgb_rec_battlemap.id, map = Rec2}};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			{ok, Req3} = cowboy_req:reply(Status, Req2),
			{halt, Req3, Ctx}
	end.

make_json(Req, Ctx, Map) ->
	{Host, Port} = Ctx#ctx.hostport,
	rpgb_rec_battlemap:make_json(Req, Host, Port, Map).

make_location(Req, Ctx, Rec) ->
	{Host, Port} = Ctx#ctx.hostport,
	rpgb:get_url(Req, Host, Port, ["map", integer_to_list(Rec#rpgb_rec_battlemap.id)]).

validate_map(Json, InitMap) ->
	ValidateFuns = [
		fun scrub_disallowed/1,
		fun check_blank_name/1,
		fun check_name_conflict/1,
		fun validate_json/1,
		fun check_named_map/1
	],
	rpgb:bind({Json, InitMap}, ValidateFuns).

check_named_map({Json, Map}) ->
	MapName = Map#rpgb_rec_battlemap.name,
	JsonName = proplists:get_value(<<"name">>, Json),
	case {MapName, JsonName} of
		{undefined, undefined} ->
			{error, 422, <<"name cannot be blank.">>};
		_ ->
			{ok, {Json, Map}}
	end.

check_blank_name({Json, Map}) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, 422, <<"name cannot be blank.">>};
		_ ->
			{ok, {Json, Map}}
	end.

check_name_conflict({Json, Map}) ->
	#rpgb_rec_battlemap{owner_id = Owner, name = MapName} = Map,
	?error("map name:  ~p;  json:  ~p", [MapName, Json]),
	case proplists:get_value(<<"name">>, Json) of
		undefined ->
			{ok, {Json, Map}};
		MapName ->
			{ok, {Json, Map}};
		OtherName ->
			Searched = rpgb_data:search(rpgb_rec_battlemap, [
				{name, OtherName}, {owner_id, Owner}]),
			case Searched of
				{ok, []} ->
					{ok, {Json, Map}};
				_ ->
					{error, 409, <<"you already have a map by that name.">>}
			end
	end.

scrub_disallowed({Json, Map}) ->
	{ok, Json2} = scrub_disallowed(Json),
	{ok, {Json2, Map}};

scrub_disallowed([{}]) ->
	{ok, [{}]};

scrub_disallowed(Json) ->
	Disallowed = [<<"id">>, <<"owner_id">>, <<"created">>, <<"updated">>,
		<<"participant_ids">>, <<"zoom">>, <<"translate_x">>,
		<<"translate_y">>, <<"grid_spacing">>, <<"bottom_layer_id">>,
		<<"first_combatant_id">>],
	Disallowed1 = ordsets:from_list(Disallowed),
	Json1 = ordsets:from_list(Json),
	scrub_disallowed(Json1, Disallowed1).

scrub_disallowed(Json, []) ->
	{ok, Json};

scrub_disallowed(Json, [Nope | Tail] = Nopes) ->
	case proplists:delete(Nope, Json) of
		Json ->
			scrub_disallowed(Json, Tail);
		Json1 ->
			scrub_disallowed(Json1, Nopes)
	end.

validate_json({Json, Map}) ->
	case validate_json(Json, Map) of
		{ok, Map2} ->
			{ok, {Json, Map2}};
		{error, {bad_color, Key}} ->
			Body = iolist_to_binary(io_lib:format("invalid color for ~s.", [Key])),
			Status = 422,
			{error, Status, Body}
	end;

validate_json(Json) ->
	case rpgb_rec_battlemap:from_json(Json) of
		{ok, Rec, Warnings} ->
			validate_warnings(Warnings, Rec);
		Else ->
			Else
	end.

validate_json(Json, Rec) ->
	{ok, Json1} = scrub_disallowed(Json),
	case rpgb_rec_battlemap:from_json(Rec, Json1) of
		{ok, Rec1, Warnings} ->
			validate_warnings(Warnings, Rec1);
		Else ->
			Else
	end.

validate_warnings([], Rec) ->
	{ok, Rec};

validate_warnings([background_color | Tail], Rec) ->
	Color = Rec#rpgb_rec_battlemap.background_color,
	case validate_color(Color) of
		true ->
			validate_warnings(Tail, Rec);
		false ->
			{error, {bad_color, background_color}}
	end;

validate_warnings([gridline_color | Tail], Rec) ->
	Color = Rec#rpgb_rec_battlemap.gridline_color,
	case validate_color(Color) of
		true ->
			validate_warnings(Tail, Rec);
		false ->
			{error, {bad_color, gridline_color}}
	end.

validate_color(Color) when is_binary(Color) ->
	true;
validate_color([_R, _G, _B] = Color) ->
	lists:all(fun
		(I) when is_integer(I) ->
			I =< 255 andalso I >= 0;
		(_) ->
			false
	end, Color);
validate_color([R, G, B, A]) when A =< 1 andalso A >= 0 ->
	validate_color([R, G, B]);
validate_color(_) ->
	false.

generate_etag(Req, #ctx{mapid = undefined} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{map = Map} = Ctx) ->
	Bin = jsx:to_json(Map:to_json()),
	Updated = Map#rpgb_rec_battlemap.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.
