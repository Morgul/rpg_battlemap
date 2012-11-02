-module(rpgb_handle_map).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, to_html/2,
	content_types_accepted/2, from_json/2, delete_resource/2]).

-record(ctx, { hostport, session, mapid, map}).

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, HostPort) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	%?debug("Session:  ~p", [Session]),
	{Path, Req2} = cowboy_http_req:path(Req1),
	%?debug("path:  ~p", [Path]),
	{MapId, Req3} = cowboy_http_req:binding(mapid, Req2),
	MapId1 = case MapId of
		undefined -> undefined;
		_ ->
			try list_to_integer(binary_to_list(MapId)) of
				N -> N
			catch
				'ERROR':{badarg_} ->
					<<"bad map id, will 404 later">>
			end
	end,
	{ok, Req3, #ctx{hostport = HostPort, session = Session, mapid = MapId1}}.

allowed_methods(Req, #ctx{mapid = undefined} = Ctx) ->
	{['GET', 'PUT', 'HEAD'], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{['GET', 'PUT', 'HEAD', 'DELETE'], Req, Ctx}.

is_authorized(Req, #ctx{mapid = MapId, session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"post">>}, Req, Ctx};
		User ->
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
				{error, not_found} ->
					{ok, Req2} = cowboy_http_req:reply(404, Req),
					{halt, Req2, Ctx}
			end
	end.

delete_resource(Req, #ctx{mapid = MapId} = Ctx) ->
	case rpgb_data:delete(rpgb_rec_battlemap, MapId) of
		{ok, _} ->
			{true, Req, Ctx};
		{error, Err} ->
			Body = iolist_to_binary(io_lib:format("Error deleting:  ~p", [Err])),
			{ok, Req1} = cowboy_http_req:set_resp_body(Body),
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

to_json(Req, Ctx) ->
	{<<"json">>, Req, Ctx}.

to_html(Req, Ctx) ->
	{<<"html">>, Req, Ctx}.

from_json(Req, #ctx{mapid = MapId} = Ctx) ->
	#ctx{session = Session} = Ctx,
	User = rpgb_session:get_user(Session),
	InitialMap = case MapId of
		undefined ->
			#rpgb_rec_battlemap{
				id = undefined,
				owner_id = User#rpgb_rec_user.id,
				participant_ids = [],
				zones = [],
				combatants = [],
				created = os:timestamp(),
				updated = os:timestamp()
			};
		_ ->
			InitM = Ctx#ctx.map,
			InitM#rpgb_rec_battlemap{updated = os:timestamp()}
	end,
	{ok, Body, Req1} = cowboy_http_req:body(Req),
	Term = jsx:to_term(Body),
	?debug("Submitted json:  ~p", [Term]),
	case validate_json(Term, InitialMap) of
		{ok, Rec} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			{Host, Port} = Ctx#ctx.hostport,
			Location = rpgb:get_url("http", Host, Port, ["map", integer_to_list(Rec2#rpgb_rec_battlemap.id)]),
			{ok, Req2} = case MapId of
				undefined ->
					cowboy_http_req:set_resp_header(<<"Location">>, Location, Req1);
				_ ->
					{ok, Req1}
			end,
			OutJson = jsx:to_json(Rec2:to_json([{<<"url">>, Location}])),
			{ok, Req3} = cowboy_http_req:set_resp_body(OutJson, Req2),
			{true, Req3, Ctx#ctx{mapid = Rec2#rpgb_rec_battlemap.id, map = Rec2}};
		{error, Error} ->
			?debug("Error:  ~p", [Error]),
			{ok, Req2} = cowboy_http_req:set_resp_body(iolist_to_binary(io_lib:format("invalid data: ~p", [Error])), Req1),
			{ok, Req3} = cowboy_http_req:reply(400, Req2),
			{halt, Req3, Ctx}
	end.

scrub_disallowed([{}]) ->
	{ok, [{}]};

scrub_disallowed(Json) ->
	Disallowed = [<<"id">>, <<"owner_id">>, <<"created">>, <<"updated">>,
		<<"participant_ids">>, <<"zoom">>, <<"translate_x">>,
		<<"translate_y">>, <<"grid_spacing">>, <<"zones">>, <<"combatants">>],
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
	Updated = Map#rpgb_rec_battlemap.created,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{Etag, Req, Ctx}.

bind(Last, []) ->
	{ok, Last};
bind(Last, [{M, F, A} | Tail]) ->
	case erlang:apply(M, F, [Last | A]) of
		{ok, Next} ->
			bind(Next, Tail);
		E ->
			E
	end.