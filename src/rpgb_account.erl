-module(rpgb_account).
-compile([export_all]).
-include_lib("webmachine/include/webmachine.hrl").
-include("log.hrl").

init(Args) ->
	?info("init"),
	{ok, Args}.

%content_types_provided(ReqData, Ctx) ->
%	?info("types provided"),
%	Types = [
%		{"text/html", to_html},
%		{"application/json", to_json}
%	],
%	{Types, ReqData, Ctx}.

%content_types_accepted(ReqData, Ctx) ->
%	?info("types accepted"),
%	Out = [{"application/x-www-form-urlencoded", handle_post}],
%	{Out, ReqData, Ctx}.

allowed_methods(ReqData, Ctx) ->
	?info("allowed methods"),
	{['GET', 'POST', 'HEAD'], ReqData, Ctx}.

%resouce_exists(ReqData, Ctx) ->
%	?info("resource exists"),
%	{false, ReqData, Ctx}.

%known_content_type(ReqData, Cts) ->
%	?info("known type"),
%	{true, ReqData, Cts}.

%post_is_create(ReqData, Ctx) ->
%	?info("post be create"),
%	{false, ReqData, Ctx}.

%allow_missing_post(ReqData, Ctx) ->
%	?info("allow missing post"),
%	{true, ReqData, Ctx}.
	
process_post(ReqData, Ctx) ->
	Post = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
	Openid = proplists:get_value("openid", Post, ""),
	?info("Openid:  ~p", [Openid]),
	SessionId0 = wrq:get_cookie_value("rpgbsid", ReqData),
	{ok, Session} = rpgb_session:get_or_create(SessionId0),
	SessionId1 = rpgb_session:get_id(Session),
	case gen_server:call(openid, {prepare, SessionId1, Openid, true}) of
		{ok, AuthReq} ->
			BaseURL = rpg_battlemap_app:get_url(),
			ReturnTo = <<BaseURL/binary, "/account/login_complete">>,
			AuthURL = openid:authentication_url(AuthReq, ReturnTo, BaseURL),
			ReqData0 = wrq:set_resp_header("Location", AuthURL, ReqData),
			ReqData1 = wrq:do_redirect(true, ReqData0),
			{true, ReqData1, Ctx};
		{error, Err} ->
			?info("Error from openid:  ~p", [Err]),
			{false, ReqData, Ctx}
	end.

to_html(ReqData, Ctx) ->
	?info("to html"),
	{ok, Out} = base_dtl:render([]),
	{Out, ReqData, Ctx}.

%to_json(ReqData, Ctx) ->
%	?info("to json"),
%	Json = {struct, [
%		{success, false}
%	]},
%	Out = mochijson2:encode(Json),
%	{Out, ReqData, Ctx}.

%handle_post(ReqData, Ctx) ->
%	?info("handle post"),
%	{<<>>, ReqData, Ctx}.
