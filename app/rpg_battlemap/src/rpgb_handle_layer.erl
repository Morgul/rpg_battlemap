-module(rpgb_handle_layer).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, resource_exists/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2
]).

-record(ctx, { hostport, session, mapid, map, layerid, layer}).

get_routes() ->
	[
		[<<"map">>, mapid, <<"layers">>],
		[<<"map">>, mapid, <<"layers">>, layerid]
	].

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, HostPort) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	%?debug("Session:  ~p", [Session]),
	{Path, Req2} = cowboy_http_req:path(Req1),
	{MapId, Req3} = cowboy_http_req:binding(mapid, Req2),
	MapId1 = case MapId of
		undefined ->
			undefined;
		_ ->
			try list_to_integer(binary_to_list(MapId)) of
				MapN ->
					MapN
			catch
				'ERROR':{badarg, _} ->
					undefined
			end
	end,
	{LayerId, Req4} = cowboy_http_req:binding(layerid, Req3),
	LayerId1 = case LayerId of
		undefined ->
			maplayers;
		_ ->
			try list_to_integer(binary_to_list(LayerId)) of
				LayerN -> LayerN
			catch
				'ERROR':{badarg, _} ->
					undefined
			end
	end,
	{ok, Req4, #ctx{hostport = HostPort, session = Session, mapid = MapId1, layerid = LayerId1}}.

allowed_methods(Req, #ctx{layerid = LayerId} = Ctx) when is_atom(LayerId) ->
	{['GET', 'PUT', 'HEAD'], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{['GET', 'PUT', 'HEAD', 'DELETE'], Req, Ctx}.

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
			{ok, Req2} = cowboy_http_req:reply(404, Req),
			{halt, Req2, Ctx};
		{ok, Map} ->
			if
				User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id ->
					{false, Req, Ctx#ctx{map = Map}};
				true ->
					{true, Req, Ctx#ctx{map = Map}}
			end
	end.

resource_exists(Req, #ctx{layerid = maplayers} = Ctx) ->
	case cowboy_http_req:method(Req) of
		{'PUT', Req2} ->
			{false, Req2, Ctx};
		{_, Req2} ->
			{true, Req, Ctx}
	end;
resource_exists(Req, #ctx{layerid = LayerId} = Ctx) ->
	{ok, Layer} = rpgb_data:get_by_id(rpgb_rec_layer, LayerId),
	{true, Req, Ctx#ctx{layer = Layer}}.

delete_resource(Req, #ctx{mapid = MapId} = Ctx) ->
	{ok, Layers} = rpgb_data:search(rpgb_rec_layer, [{battlemap_id, MapId}]),
	?debug("Found layers ~p", [Layers]),
	case Layers of
		[_Elem] ->
			{ok, Req2} = cowboy_http_req:set_resp_body(<<"you cannot delete the last layer of a map">>, Req),
			{ok, Req3} = cowboy_http_req:reply(422, Req2),
			{halt, Req3, Ctx};
		_ ->
			#ctx{map = Map, layerid = LayerId, layer = Layer} = Ctx,
			case Map#rpgb_rec_battlemap.bottom_layer_id of
				LayerId ->
					Map1 = Map#rpgb_rec_battlemap{bottom_layer_id = Layer#rpgb_rec_layer.next_layer_id},
					rpgb_data:save(Map1);
				_ ->
					{ok, [PrevLayer | _]} = rpgb_data:search(rpgb_rec_layer, [{next_layer_id, LayerId}]),
					PrevLayer2 = PrevLayer#rpgb_rec_layer{next_layer_id = Layer#rpgb_rec_layer.next_layer_id},
					{ok, _PrevLayer3} = rpgb_data:save(PrevLayer2)
			end,
			rpgb_data:delete(Layer),
			{true, Req, Ctx}
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

to_json(Req, #ctx{layerid = maplayers} = Ctx) ->
	#ctx{map = MapRec} = Ctx,
	#rpgb_rec_battlemap{bottom_layer_id = FirstLayerId} = MapRec,
	Layers = get_layers(FirstLayerId),
	Json = [make_json(Req, Ctx, Layer) || Layer <- Layers],
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{layer = Layer} = Ctx) ->
	Json = make_json(Req, Ctx, Layer),
	{jsx:to_json(Json), Req, Ctx}.

from_json(Req, #ctx{layerid = maplayers} = Ctx) ->
	#ctx{session = Session, map = Map, mapid = MapId} = Ctx,
	User = rpgb_session:get_user(Session),
	InitialLayer = #rpgb_rec_layer{
		id = undefined, name = <<>>, battlemap_id = MapId, created = os:timestamp(),
		updated = os:timestamp()
	},
	{ok, Body, Req1} = cowboy_http_req:body(Req),
	Term = jsx:to_term(Body),
	case validate_layer(Term, InitialLayer) of
		{ok, {_Json, Rec}} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			Rec3 = insert_layer(Map, Rec2),
			Ctx2 = Ctx#ctx{layer = Rec3, layerid = Rec3#rpgb_rec_layer.id},
			Location = make_location(Req1, Ctx2, Rec3),
			{ok, Req2} = cowboy_http_req:set_resp_header(<<"Location">>, Location, Req1),
			{OutBody, Req3, Ctx3} = to_json(Req2, Ctx2),
			{ok, Req4} = cowboy_http_req:set_resp_body(OutBody, Req3),
			{true, Req4, Ctx3};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			{ok, Req2} = cowboy_http_req:set_resp_body(ErrBody2, Req1),
			{ok, Req3} = cowboy_http_req:reply(Status, Req2),
			{halt, Req3, Ctx}
	end;

from_json(Req, #ctx{mapid = MapId, map = Map, layer = InitL} = Ctx) ->
	#ctx{session = Session} = Ctx,
	User = rpgb_session:get_user(Session),
	InitialLayer = InitL#rpgb_rec_layer{updated = os:timestamp()},
	{ok, Body, Req1} = cowboy_http_req:body(Req),
	Term = jsx:to_term(Body),
	?debug("Submitted json:  ~p", [Term]),
	case validate_layer(Term, InitialLayer) of
		{ok, {_DerJson, Rec}} ->
			remove_layer(Map, InitialLayer),
			{ok, Rec3} = rpgb_data:save(Rec),
			{ok, Map2} = rpgb_data:get_by_id(rpgb_rec_battlemap, Map#rpgb_rec_battlemap.id),
			Rec4 = insert_layer(Map2, Rec3),
			Map3 = rpgb_data:get_by_id(rpgb_rec_battlemap, Map2#rpgb_rec_battlemap.id),
			Ctx2 = Ctx#ctx{layer = Rec4, map = Map3},
			{OutBody, Req2, Ctx3} = to_json(Req1, Ctx2),
			{ok, Req3} = cowboy_http_req:set_resp_body(OutBody, Req2),
			{true, Req3, Ctx3};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			{ok, Req2} = cowboy_http_req:set_resp_body(ErrBody2, Req1),
			{ok, Req3} = cowboy_http_req:reply(Status, Req2),
			{halt, Req3, Ctx}
	end.

get_layers(undefined) ->
	[];
get_layers(Id) ->
	get_layers(Id, []).

get_layers(undefined, Acc) ->
	lists:reverse(Acc);
get_layers(Id, Acc) ->
	{ok, Layer} = rpgb_data:get_by_id(rpgb_rec_layer, Id),
	#rpgb_rec_layer{next_layer_id = NextId} = Layer,
	get_layers(NextId, [Layer | Acc]).

make_json(Req, Ctx, Layer) ->
	{Host, Port} = Ctx#ctx.hostport,
	Proto = case cowboy_http_req:transport(Req) of
		{ok, cowboy_ssl_transport, _} ->
			https;
		_ ->
			http
	end,
	rpgb_layer:make_json(Proto, Host, Port, Layer).

make_location(Req, Ctx, Rec) ->
	{Host, Port} = Ctx#ctx.hostport,
	rpgb:get_url(Req, Host, Port, ["map", integer_to_list(Rec#rpgb_rec_layer.battlemap_id), "layers", integer_to_list(Rec#rpgb_rec_layer.id)]).

validate_layer(Json, InitLayer) ->
	ValidateFuns = [
		fun scrub_disallowed/1,
		fun check_blank_name/1,
		fun check_name_conflict/1,
		fun validate_json/1,
		fun check_named_layer/1,
		fun check_next_layer_id/1,
		fun check_next_layer_self/1
	],
	rpgb:bind({Json, InitLayer}, ValidateFuns).

check_next_layer_self({Json, Layer}) ->
	#rpgb_rec_layer{id = LayerId} = Layer,
	case proplists:get_value(<<"next_layer_id">>, Json) of
		LayerId when LayerId =/= undefined ->
			{error, 422, <<"layer cannot point to itself">>};
		_ ->
			{ok, {Json, Layer}}
	end.

check_next_layer_id({Json, Layer}) ->
	NextId = proplists:get_value(<<"next_layer_id">>, Json),
	case NextId of
		null ->
			{ok, {Json, Layer#rpgb_rec_layer{next_layer_id = undefined}}};
		undefined ->
			{ok, {Json, Layer}};
		_ ->
			#rpgb_rec_layer{battlemap_id = MapId} = Layer,
			case rpgb_data:search(rpgb_rec_layer, [{battlemap_id, MapId},{id, NextId}]) of
				{ok, []} ->
					{error, 422, <<"layer set as next does not exist">>};
				_ ->
					{ok, {Json, Layer#rpgb_rec_layer{next_layer_id = NextId}}}
			end
	end.

check_named_layer({Json, Layer}) ->
	LayerName = Layer#rpgb_rec_layer.name,
	JsonName = proplists:get_value(<<"name">>, Json),
	case {LayerName, JsonName} of
		{undefined, undefined} ->
			{error, 422, <<"name cannot be blank">>};
		_ ->
			{ok, {Json, Layer}}
	end.

check_blank_name({Json, Layer}) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, 422, <<"name cannot be blank.">>};
		_ ->
			{ok, {Json, Layer}}
	end.

check_name_conflict({Json, Layer}) ->
	#rpgb_rec_layer{battlemap_id = MapId, name = LayerName} = Layer,
	case proplists:get_value(<<"name">>, Json) of
		undefined ->
			{ok, {Json, Layer}};
		LayerName ->
			{ok, {Json, Layer}};
		OtherName ->
			Searched = rpgb_data:search(rpgb_rec_layer, [
				{name, OtherName}, {battlemap_id, MapId}]),
			case Searched of
				{ok, []} ->
					{ok, {Json, Layer}};
				_ ->
					{error, 409, <<"you already have a layer by that name.">>}
			end
	end.

scrub_disallowed({Json, Map}) ->
	{ok, Json2} = scrub_disallowed(Json),
	{ok, {Json2, Map}};

scrub_disallowed([{}]) ->
	{ok, [{}]};

scrub_disallowed(Json) ->
	Disallowed = [<<"id">>, <<"battlemap_id">>, <<"created">>, <<"updated">>],
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

validate_json({Json, Layer}) ->
	case Layer:from_json(Json) of
		{ok, Layer2} ->
			{ok, {Json, Layer2}};
		{ok, #rpgb_rec_layer{next_layer_id = null} = Layer2, [next_layer_id]} ->
			{ok, {Json, Layer2}};
		{_, Else} ->
			Body = iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Else])),
			{error, 422, Body}
	end.

generate_etag(Req, #ctx{mapid = undefined} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{map = Map} = Ctx) ->
	Bin = jsx:to_json(Map:to_json()),
	Updated = Map#rpgb_rec_battlemap.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.

remove_layer(_Map, #rpgb_rec_layer{id = undefined} = Layer) ->
	Layer;
remove_layer(#rpgb_rec_battlemap{bottom_layer_id = Id} = Map, #rpgb_rec_layer{id = Id} = Layer) ->
	NextId = Layer#rpgb_rec_layer.next_layer_id,
	Map2 = Map#rpgb_rec_battlemap{bottom_layer_id = NextId},
	{ok, _Map3} = rpgb_data:save(Map2),
	Layer;
remove_layer(_Map, #rpgb_rec_layer{id = Id, next_layer_id = NextId} = Layer) ->
	case rpgb_data:search(rpgb_rec_layer, [{next_layer_id, Id}]) of
		{ok, []} ->
			ok;
		{ok, [PrevLayer | _]} ->
			PrevLayer2 = PrevLayer#rpgb_rec_layer{next_layer_id = NextId},
			rpgb_data:save(PrevLayer2)
	end,
	Layer.

insert_layer(_Map, #rpgb_rec_layer{id = undefined}) ->
	erlang:error(badarg);
insert_layer(#rpgb_rec_battlemap{bottom_layer_id = undefined} = Map, Layer) ->
	Map2 = Map#rpgb_rec_battlemap{bottom_layer_id = Layer#rpgb_rec_layer.id},
	rpgb_data:save(Map2),
	Layer;
insert_layer(#rpgb_rec_battlemap{bottom_layer_id = NextId} = Map, #rpgb_rec_layer{next_layer_id = NextId, id = Id} = Layer) ->
	Map2 = Map#rpgb_rec_battlemap{bottom_layer_id = Id},
	rpgb_data:save(Map2),
	Layer;
insert_layer(_Map, #rpgb_rec_layer{id = Id} = Layer) ->
	NextId = Layer#rpgb_rec_layer.next_layer_id,
	{ok, PrevLayers} = rpgb_data:search(rpgb_rec_layer, [{next_layer_id, NextId}]),
	case [L || #rpgb_rec_layer{id = Lid} = L <- PrevLayers, Lid =/= Id] of
		[] ->
			ok;
		[PrevLayer | _] ->
			PrevLayer2 = PrevLayer#rpgb_rec_layer{next_layer_id = Id},
			rpgb_data:save(PrevLayer2)
	end,
	Layer.