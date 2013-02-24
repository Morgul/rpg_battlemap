-module(rpgb_handle_template).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([init/3, handle/2, terminate/2]).

init(_Transport, Req, Ctx) ->
	{ok, Req, Ctx}.

handle(Req, {{Host, Port}, Template} = Ctx) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	User = rpgb_session:get_user(Session),
	LoginLink = rpgb:get_url(Host, Port, ["account"]),
	LogoutLink = rpgb:get_url(Host, Port, ["account", "logout"]),
	{ok, Output} = Template:render([
		{user, User}, {login_link, LoginLink}, {logout_link, LogoutLink}
	]),
	{ok, Req2} = cowboy_req:reply(200, [], Output, Req1),
	{ok, Req2, Ctx}.

terminate(Req, Ctx) ->
	ok.
