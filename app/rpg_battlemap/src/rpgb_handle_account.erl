-module(rpgb_handle_account).

-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2]).

-record(ctx, {
	hostport,
	session,
	action
}).

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, HostPort) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{Path, Req2} = cowboy_http_req:path(Req1),
	Action= case path of
		<<"login_complete">> ->
			login;
		_ ->
			undefined
	end,
	{ok, Req1, #ctx{hostport = HostPort, session = Session, action = Action}}.

allowed_methods(Req, Ctx) ->
	{['GET', 'POST', 'HEAD'], Req, Ctx}.

is_authorized(Req, #ctx{action = undefined, session = Session} = Ctx) ->
	case cowboy_http_req:method(Req) of
		{'POST', Req1} ->
			{true, Req1, Ctx};
		{_, Req1} ->
			{{false, <<"post">>}, Req1, Ctx}
	end.
	
%resource_exists(Req, #ctx{action = undefined} = Ctx) ->
%	{true, Req, Ctx};
%resource_exists(Req, Ctx) ->
%	{false, Req, Ctx}.

