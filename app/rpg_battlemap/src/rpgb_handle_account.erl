-module(rpgb_handle_account).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	content_types_provided/2, process_post/2]).

-record(ctx, {
	hostport,
	session,
	action
}).

get_routes() ->
	[
		[<<"account">>],
		[<<"account">>, <<"login">>],
		[<<"account">>, <<"logout">>]
	].

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_http_rest}.

rest_init(Req, HostPort) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{Path, Req2} = cowboy_http_req:path(Req1),
	?debug("path:  ~p", [Path]),
	Action= case Path of
		[_, <<"login">>] ->
			login;
		[_, <<"logout">>] ->
			logout;
		_ ->
			undefined
	end,
	{ok, Req1, #ctx{hostport = HostPort, session = Session, action = Action}}.

allowed_methods(Req, Ctx) ->
	{['GET', 'POST', 'HEAD'], Req, Ctx}.

is_authorized(Req, #ctx{action = Action} = Ctx) when Action =:= login; Action =:= logout ->
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

process_post(Req, #ctx{session = Session, hostport = {Host, Port}, action = logout} = Ctx) ->
	?info("processing logout"),
	Req1 = rpgb_session:destroy(Req),
	%{ok, Req2} = cowboy_http_req:reply(200, Req1),
	%{halt, Req2, Ctx};
	{true, Req1, Ctx};

process_post(Req, #ctx{session = Session, hostport = {Host, Port}, action = login} = Ctx) ->
	?info("processing login"),
	SessionId = rpgb_session:get_id(Session),
	BaseURL = rpgb:get_url(Req, []),
	{ok, Post, Req1} = cowboy_http_req:body(Req),
	Json = jsx:to_term(Post),
	?info("Json term:  ~p", [Json]),
	Assertion = proplists:get_value(<<"assertion">>, Json),
	AssertBody = jsx:to_json([{<<"assertion">>, Assertion},
		{<<"audience">>, BaseURL}]),
	Asserted = ibrowse:send_req("https://verifier.login.persona.org/verify",
		[{"Content-Type", "application/json"}], post, AssertBody),
	case Asserted of
		{ok, "200", _Heads, AssertedBody} ->
			AssertedJson = jsx:to_term(list_to_binary(AssertedBody)),
			Email = proplists:get_value(<<"email">>, AssertedJson),
			case proplists:get_value(<<"status">>, AssertedJson) of
				<<"okay">> ->
					{ok, Session1} = case rpgb_data:search(rpgb_rec_user, [{email, Email}]) of
						{ok, []} ->
							?info("Creating new user ~p", [Email]),
							Userrec = #rpgb_rec_user{ email = Email, group_id = 1 },
							{ok, Userrec1} = rpgb_data:save(Userrec),
							rpgb_session:set_user(Userrec1, Session);
						{ok, Userrecs} ->
							[Userrec | Destroy] = lists:keysort(2, Userrecs),
							spawn(fun() ->
								[rpgb_data:delete(R) || R <- Destroy]
							end),
							?info("existant user ~p", [Userrec]),
							rpgb_session:set_user(Userrec, Session)
					end,
					{true, Req1, Ctx#ctx{session = Session1}};
				_ ->
					{false, Req1, Ctx}
			end;
		_ ->
			?info("Verifier failed:  ~p", [Asserted]),
			{false, Req1, Ctx}
	end.
