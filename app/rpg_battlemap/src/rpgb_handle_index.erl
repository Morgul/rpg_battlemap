-module(rpgb_handle_index).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, handle/2, terminate/3]).

get_routes() -> [<<"/">>].

init(_Transport, Req, Ctx) ->
	{ok, Req, Ctx}.

handle(Req, [{Host, Port}] = Ctx) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	User = rpgb_session:get_user(Session),
	LoginLink = rpgb:get_url(Req, ["account", "login"]),
	LogoutLink = rpgb:get_url(Req, ["account", "logout"]),
	MapCreateUrl = rpgb:get_url(Req, ["map"]),
	CharacterCreateUrl = rpgb:get_url(Req, ["character"]),
	{NoUrlMaps, NoUrlCharacters, NoUrlParticipantMaps} = case User of
		undefined ->
			{[], [], []};
		_ ->
			{ok, OutMaps} = rpgb_data:search(rpgb_rec_battlemap, [
				{owner_id, User#rpgb_rec_user.id}
			]),
			{ok, OutChars} = rpgb_data:search(rpgb_rec_character, [
				{owner_id, User#rpgb_rec_user.id}
			]),
			{ok, OutParticipantMaps} = rpgb_rec_battlemap:get_by_participant(User),
			{OutMaps, OutChars, OutParticipantMaps}
	end,
	Maps = add_map_urls(NoUrlMaps, Req),
	Characters =  add_character_urls(NoUrlCharacters, Req),
	ParticipantMaps = add_map_urls(NoUrlParticipantMaps, Req),
	rpgb:refresh_templates(index_dtl),
	{ok, Output} = index_dtl:render([
		{user, User}, {login_link, LoginLink}, {logout_link, LogoutLink},
		{maps, Maps}, {map_create_url, MapCreateUrl}, {characters, Characters},
		{character_create_url, CharacterCreateUrl}, {member_of_maps, ParticipantMaps}
	]),
	{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Output, Req1),
	{ok, Req2, Ctx}.

terminate(_Cause, _Req, _Ctx) ->
	ok.

add_map_urls(Maps, Req) ->
	add_map_urls(Maps, Req, []).

add_map_urls([], _Req, Acc) ->
	lists:reverse(Acc);

add_map_urls([Map | Tail], Req, Acc) ->
	Props = Map:to_json([fun(Json, Rec) ->
		Url = rpgb:get_url(Req, ["map", integer_to_list(Map#rpgb_rec_battlemap.id)]),
		[{url, Url} | Json]
	end ]),
	add_map_urls(Tail, Req, [Props | Acc]).

add_character_urls(Characters, Req) ->
	add_character_urls(Characters, Req, []).

add_character_urls([], _Req, Acc) ->
	lists:reverse(Acc);

add_character_urls([Character | Tail], Req, Acc) ->
	Props = Character:to_json([fun(Json, Rec) ->
		Url = rpgb:get_url(Req, ["character", integer_to_list(Character#rpgb_rec_character.id)]),
		[{url, Url} | Json]
	end ]),
	add_character_urls(Tail, Req, [Props | Acc]).
