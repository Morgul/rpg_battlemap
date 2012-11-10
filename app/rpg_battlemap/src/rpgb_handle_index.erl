-module(rpgb_handle_index).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, handle/2, terminate/2]).

get_routes() -> [[]].

init(_Transport, Req, Ctx) ->
	{ok, Req, Ctx}.

handle(Req, {Host, Port} = Ctx) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	User = rpgb_session:get_user(Session),
	LoginLink = rpgb:get_url(Req, ["account", "login"]),
	LogoutLink = rpgb:get_url(Req, ["account", "logout"]),
	rpgb:refresh_templates(index_dtl),
	{ok, Output} = index_dtl:render([
		{user, User}, {login_link, LoginLink}, {logout_link, LogoutLink}
	]),
	{ok, Req2} = cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Output, Req1),
	{ok, Req2, Ctx}.

terminate(Req, Ctx) ->
	ok.
