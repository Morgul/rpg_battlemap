-module(rpgb_handle_account).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	content_types_provided/2, process_post/2, to_html/2]).

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
	?debug("path:  ~p", [Path]),
	Action= case Path of
		[_, <<"login_complete">>] ->
			login;
		_ ->
			undefined
	end,
	{ok, Req1, #ctx{hostport = HostPort, session = Session, action = Action}}.

allowed_methods(Req, Ctx) ->
	{['GET', 'POST', 'HEAD'], Req, Ctx}.

is_authorized(Req, #ctx{action = login} = Ctx) ->
	{true, Req, Ctx};
is_authorized(Req, #ctx{action = undefined, session = Session} = Ctx) ->
	case cowboy_http_req:method(Req) of
		{'POST', Req1} ->
			?debug("authorized"),
			{true, Req1, Ctx};
		{_, Req1} ->
			?debug("not authorized, post to me"),
			{{false, <<"post">>}, Req1, Ctx}
	end.

content_types_provided(Req, Ctx) ->
	?info("content types provided"),
	Types = [
		{{<<"text">>, <<"html">>, []}, to_html},
		{{<<"text">>, <<"json">>, []}, to_json}
	],
	{Types, Req, Ctx}.

resource_exists(Req, #ctx{action = undefined} = Ctx) ->
	{true, Req, Ctx};
resource_exists(Req, Ctx) ->
	{false, Req, Ctx}.

previously_existed(Req, Ctx) ->
	{true, Req, Ctx}.

process_post(Req, #ctx{session = Session, hostport = {Host, Port}} = Ctx) ->
	?info("processing a likely login"),
	{Post, Req1} = cowboy_http_req:body_qs(Req),
	Openid = proplists:get_value(<<"openid">>, Post, <<>>),
	?debug("The body:  ~p", [Post]),
	?info("Openid:  ~p", [Openid]),
	SessionId = rpgb_session:get_id(Session),
	try openid:prepare(SessionId, binary_to_list(Openid)) of
		{ok, AuthReq} ->
			BaseURL = rpgb:get_url(Host, Port),
			ReturnTo = <<BaseURL/binary, "account/login_complete">>,
			AuthURL = openid:authentication_url(AuthReq, binary_to_list(ReturnTo), binary_to_list(BaseURL), [
				{"openid.sreg.optional", "nickname"}]),
			{ok, Req2} = cowboy_http_req:set_resp_header(<<"Location">>, AuthURL, Req1),
			{ok, Req3} = cowboy_http_req:reply(303, Req2),
			{halt, Req3, Ctx};
		{error, Err} ->
			?info("Error from openid: ~p", [Err]),
			{false, Req1, Ctx}
	catch
		'EXIT':ExitY ->
			?info("Exit from openid: ~p", [ExitY]),
			{false, Req1, Ctx}
	end.

to_html(Req, #ctx{action = login, session = Session, hostport = {Host, Port}} = Ctx) ->
	SessionId = rpgb_session:get_id(Session),
	BaseURL = rpgb:get_url(Host, Port),
	ReturnTo = <<BaseURL/binary, "account/login_complete">>,
	{QueryParams, Req1} = cowboy_http_req:qs_vals(Req),
	?debug("query params:  ~p", [QueryParams]),
	QueryParams1 = [ {binary_to_list(K), binary_to_list(V)} || {K, V} <- QueryParams],
	case openid:verify(SessionId, binary_to_list(ReturnTo), QueryParams) of
		{ok, OpenID} ->
			Username = proplists:get_value(<<"openid.sreg.nickname">>, QueryParams, <<"Awesome User">>),
			OpenID1 = list_to_binary(OpenID),
			{ok, Session1} = case rpgb_data:search(rpgb_rec_user, [{openid, OpenID1}]) of
				{ok, []} ->
					?info("Creating new user for openid ~p", [OpenID1]),
					Userrec = #rpgb_rec_user{
						openid = OpenID1,
						name = Username,
						group_id = 1
					},
					{ok, Userrec1} = rpgb_data:save(Userrec),
					rpgb_session:set_user(Userrec1, Session);
				{ok, [Userrec]} ->
					?info("existant user ~p", [Userrec]),
					rpgb_session:set_user(Userrec, Session)
			end,
			{ok, Req2} = cowboy_http_req:set_resp_header(<<"Location">>, <<"/">>, Req1),
			{ok, Req3} = cowboy_http_req:reply(303, Req2),
			{halt, Req3, Ctx};
		{error, Fail} ->
			?info("Failure handling login:  ~p", [Fail]),
			{ok, Req, Ctx}
	end.
