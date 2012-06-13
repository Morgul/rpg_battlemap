-module(rpgb_battles).
-compile([export_all]).
-include_lib("webmachine/include/webmachine.hrl").
-include("log.hrl").

-type mode() :: 'zone' | 'zones' | 'combatant' | 'combatants'.
init(Mode) ->
	rpgb:res_init(Mode).

allowed_methods(ReqData, search_battles) ->
	?info("allowed methods"),
	{ok, Session, ReqData0} = rpgb_session:get_or_create(ReqData),
	{['GET','HEAD'], ReqData0, {search_battles, Session}};

allowed_methods(ReqData, Mode) ->
	?info("allowed methods"),
	{ok, Session, ReqData0} = rpgb_session:get_or_create(ReqData),
	Methods = case Mode of
		create_battle ->
			['POST', 'HEAD'];
		battle ->
			['GET','PUT', 'DELETE', 'HEAD']
	end,
	{Methods, ReqData0, {Mode, Session}}.

is_authorized(ReqData, {search_battles, _Session} = Ctx) ->
	{true, ReqData, Ctx};

is_authorized(ReqData, {_Mode, Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{"openid", ReqData, Ctx};
		_User ->
			{true, ReqData, Ctx}
	end.

forbidden(ReqData, {search_battles, _} = Ctx) ->
	{false, ReqData, Ctx};

forbidden(ReqData, {create_battle, _} = Ctx) ->
	{false, ReqData, Ctx};

forbidden(ReqData, {battle, Session} = Ctx) ->
	case wrq:path_info(battle_id, ReqData) of
		undefined ->
			{false, ReqData, Ctx};
		MapId ->
			case boss_db:find(MapId) of
				{error, Reason} ->
					?info("Error finding battlemap ~p:  ~p", [MapId, Reason]),
					{false, ReqData, {battle, MapId, Session}};
				undefined ->
					?info("Didn't find the battlemap ~p", [MapId]),
					{false, ReqData, {battle, MapId, Session}};
				BattleMap ->
					SessionUser = rpgb_session:get_user(Session),
					Uid = proplists:get_value(id, SessionUser),
					case BattleMap:owner_id() of
						Uid ->
							{false, ReqData, {battle, MapId, Session}};
						_ ->
							{true, ReqData, {battle, MapId, Session}}
					end
			end
	end.

is_conflict(ReqData, {battle, BattleMap, Session} = Ctx) ->
	Body = wrq:req_body(ReqData),
	{struct, Props} = mochijson2:decode(Body),
	Name = proplists:get_value(<<"name">>, Props),
	case BattleMap:name() of
		Name ->
			{false, ReqData, Ctx};
		OtherName ->
			case boss_db:find(rpgb_battlemap, [{name, equals, Name}, {owner_id, equals, BattleMap:owner_id()}]) of
				[] ->
					{false, ReqData, Ctx};
				_ ->
					?info("A map named ~p already exists for the user ~p", [OtherName, BattleMap:owner_id()]),
					{true, ReqData, Ctx}
		end
	end.

content_types_accepted(ReqData, Ctx) ->
	Types = [{"application/json", from_json}],
	{Types, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
	Types = [
		{"application/json",to_json},
		{"text/html", to_html}
	],
	{Types, ReqData, Ctx}.

resource_exists(ReqData, {search_battles, _} = Ctx) ->
	{true, ReqData, Ctx};

resource_exists(ReqData, {create_battle, _} = Ctx) ->
	{false, ReqData, Ctx};

resource_exists(ReqData, {battle, MapId, Session} = Ctx) ->
	case boss_db:find(MapId) of
		{error, Reason} ->
			?info("Could not find map ~p", [MapId]),
			{false, ReqData, Ctx};
		undefined ->
			{false, ReqData, Ctx};
		BattleMap ->
			Ctx0 = {battle, BattleMap, Session},
			{true, ReqData, Ctx0}
	end.

allow_missing_post(ReqData, {create_battle, _} = Ctx) ->
	{true, ReqData, Ctx};

allow_missing_post(ReqData, Ctx) ->
	{false, ReqData, Ctx}.

delete_resource(ReqData, {battle, MapId, Session} = Ctx) ->
	case boss_db:delete(MapId:id()) of
		ok -> {true, ReqData, Ctx};
		{error, Err} ->
			?warning("Could not delete ~s due to ~p", [MapId, Err]),
			{false, ReqData, Ctx}
	end.

process_post(ReqData, {create_battle, Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	Userid = proplists:get_value(id, User),
	Body = wrq:req_body(ReqData),
	{struct, Props} = mochijson2:decode(Body),
	Name = proplists:get_value(<<"name">>, Props),
	BattleMap = boss_record:new(rpgb_battlemap, [
		{name, Name},
		{json, Body},
		{owner_id, Userid}
	]),
	case boss_db:find(rpgb_battlemap, [{name, equals, Name},{owner_id,equals,Userid}]) of
		[] ->
			{ok, BattleMap0} = BattleMap:save(),
			NameSlug = rpgb:sluggify(Name),
			Uri = io_lib:format("/battles/~s/~s", [BattleMap0:id(),NameSlug]),
			{true, ReqData, {create_battle, Uri, Session}};
		_Recs ->
			?info("Creation failed, map named ~p already exists for user ~p", [Name, Userid]),
			ReqData0 = wrq:set_resp_body(iolist_to_binary(mochijson2:encode(<<"map with given name already exists">>)), ReqData),
			{{halt, 409}, ReqData0, Ctx}
	end.

post_is_create(ReqData, {create_battle, Session} = Ctx) ->
	{false, ReqData, Ctx}.

create_path(ReqData, {create_battle, Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	Userid = proplists:get_value(id, User),
	Body = wrq:req_body(ReqData),
	{struct, Props} = mochijson2:decode(Body),
	Name = proplists:get_value(<<"name">>, Props),
	BattleMap = boss_record:new(rpgb_battlemap, [
		{name, Name},
		{json, Body},
		{owner_id, Userid}
	]),
	{ok, BattleMap0} = BattleMap:save(),
	NameSlug = rpgb:sluggify(Name),
	Uri = io_lib:format("/battles/~s/~s", [BattleMap0:id(),NameSlug]),
	{Uri, ReqData, Ctx}.

generate_etag(ReqData, {battle, MapId, Session}) when is_list(MapId) ->
	BattleMap = boss_db:find(MapId),
	generate_etag(ReqData, {battle, BattleMap, Session});

generate_etag(ReqData, {battle, BattleMap, Session} = Ctx) ->
	Iolist = io_lib:format("~p:~p", [BattleMap:id(), BattleMap:updated_time()]),
	IoBin = iolist_to_binary(Iolist),
	Md5 = erlang:md5(IoBin),
	Etag = mochihex:to_hex(Md5),
	{Etag, ReqData, Ctx};

generate_etag(ReqData, Ctx) ->
	{undefined, ReqData, Ctx}.
	
from_json(ReqData, {create_battle, Session} = Ctx) ->
	% by the time we get here, it's actually already been created.
	% so just act cool.
	{true, ReqData, Ctx};

from_json(ReqData, {battle, MapId, Session}) when is_list(MapId) ->
	BattleMap = boss_db:find(MapId),
	from_json(ReqData, {battle, BattleMap, Session});

from_json(ReqData, {battle, BattleMap, Session} = Ctx) ->
	Body = wrq:req_body(ReqData),
	{struct, Props} = mochijson2:decode(Body),
	Name = proplists:get_value(<<"name">>, Props),
	{struct, Props} = mochijson2:decode(Body),
	BattleMap0 = BattleMap:set([{name, Name},{json, Body}]),
	{ok, BattleMap1} = BattleMap0:save(),
	{true, ReqData, {battle, BattleMap1, Session}}.

to_json(ReqData, {search_battles, Session} = Ctx) ->
	Limit0 = list_to_integer(wrq:get_qs_value("limit", "100",ReqData)),
	Limit = if
		Limit0 > 10000 -> 1000;
		Limit0 < 1 -> 1;
		true -> Limit0
	end,
	User = case rpgb_session:get_user(Session) of
		undefined -> [];
		E -> E
	end,
	Conditions = [{owner_id, equals, proplists:get_value(id, User)}],
	Records = boss_db:find(rpgb_battlemap, Conditions, Limit),
	Jsons = [begin
		Url = rpgb:get_url(["battles",Record:id(),"slug"]),
		Name = Record:name(),
		{Etag, _, _} = generate_etag(ReqData, {battle, Record, Session}),
		{struct, [
			{<<"url">>, Url},
			{<<"name">>, Name},
			{<<"etag">>, list_to_binary(Etag)}
		]}
	end || Record <- Records],
	{mochijson2:encode(Jsons), ReqData, Ctx};

to_json(ReqData, {battle, MapId, Session}) when is_list(MapId) ->
	BattleMap = boss_db:find(MapId),
	to_json(ReqData, {battle, BattleMap, Session});

to_json(ReqData, {battle, BattleMap, Session} = Ctx) ->
	Json = encode_map(BattleMap),
	{mochijson2:encode(Json), ReqData, Ctx}.

encode_map(BattleMap) ->
	{struct, MapStruct} = mochijson2:decode(BattleMap:json()),
	Url = rpgb:get_url(["battles",BattleMap:id(),"slug"]),
	{struct, [{<<"url">>, Url} | proplists:delete(<<"url">>, MapStruct)]}.

to_html(ReqData, {battle, MapId, Session}) when is_list(MapId) ->
	BattleMap = boss_db:find(MapId),
	to_html(ReqData, {battle, BattleMap, Session});

to_html(ReqData, {battle, BattleMap, Session} = Ctx) ->
	Json = encode_map(BattleMap),
	Templatevars = [
		{"session", rpgb_session:to_dict(Session)},
		{"json", mochijson2:encode(Json)},
		{"battlemap", BattleMap}
	],
	{ok, Out} = battlemap_dtl:render(Templatevars),
	{Out, ReqData, Ctx}.
