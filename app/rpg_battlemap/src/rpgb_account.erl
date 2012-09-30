-module(rpgb_account).
-compile([export_all]).
-include_lib("webmachine/include/webmachine.hrl").
-include("log.hrl").
-include("rpg_battlemap.hrl").

init(Args) ->
	?info("init"),
	rpgb:res_init(Args).

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

allowed_methods(ReqData, _Ctx) ->
	?info("allowed methods"),
	{ok, Session, ReqData0} = rpgb_session:get_or_create(ReqData),
	{['GET', 'POST', 'HEAD'], ReqData0, Session}.

resource_exists(ReqData, Ctx) ->
	?info("resource exists"),
	case wrq:path_info(action, ReqData) of
		"login_complete" ->
			{false, ReqData, Ctx};
		_ ->
			{true, ReqData, Ctx}
	end.

previously_existed(ReqData, Ctx) ->
	{true, ReqData, Ctx}.

moved_temporarily(ReqData, Session) ->
	SessionId = rpgb_session:get_id(Session),
	BaseURL = rpgb:get_url(),
	ReturnTo = <<BaseURL/binary, "/account/login_complete">>,
	QueryParams = wrq:req_qs(ReqData),
	case gen_server:call(openid, {verify, SessionId, binary_to_list(ReturnTo), QueryParams}) of
		{ok, OpenID} ->
			?info("We have ourselves a user:  ~p", [OpenID]),
			?info("query params:  ~p", [QueryParams]),
			Username = proplists:get_value("openid.sreg.nickname", QueryParams, "Awesome User"),
			{ok, Session1} = case rpgb_data:get_web_user_by_openid(list_to_binary(OpenID)) of
				notfound ->
					Userrec = #rpgb_rec_user{
						openid = list_to_binary(OpenID),
						name = list_to_binary(Username),
						group_id = 1
					},
					?info("Das user:  ~p\n", [Userrec]),
					{ok, Userrec0} = rpgb_data:save_web_user(Userrec),
					rpgb_session:set_user(Userrec0, Session);
				{ok, Userrec} ->
					rpgb_session:set_user(Userrec, Session)
			end,
			ReqData1 = wrq:do_redirect(true, wrq:set_resp_header("Location", "/", ReqData)),
			{{true, "/"}, ReqData1, Session1};
		{error, Fail} ->
			?info("And it's all fucked up:  ~p", [Fail]),
			{{true, "/"}, ReqData, Session}
	end.

%known_content_type(ReqData, Cts) ->
%	?info("known type"),
%	{true, ReqData, Cts}.

%post_is_create(ReqData, Ctx) ->
%	?info("post be create"),
%	{false, ReqData, Ctx}.

%allow_missing_post(ReqData, Ctx) ->
%	?info("allow missing post"),
%	{true, ReqData, Ctx}.
	
process_post(ReqData, Session) ->
	?info("processing a likely login"),
	Post = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
	Openid = proplists:get_value("openid", Post, ""),
	?info("Openid:  ~p", [Openid]),
	SessionId = rpgb_session:get_id(Session),
	try gen_server:call(openid, {prepare, SessionId, Openid, true}) of
		{ok, AuthReq} ->
			BaseURL = rpgb:get_url(),
			ReturnTo = <<BaseURL/binary, "/account/login_complete">>,
			AuthURL = openid:authentication_url(AuthReq, ReturnTo, BaseURL, [
				{"openid.sreg.optional", "nickname"}]),
			ReqData1 = wrq:set_resp_header("Location", binary_to_list(AuthURL), ReqData),
			ReqData2 = wrq:do_redirect(true, ReqData1),
			{true, ReqData2, Session};
		{error, Err} ->
			?info("Error from openid:  ~p", [Err]),
			{false, ReqData, Session}
	catch
		'EXIT':ExitY ->
			?info("Exit from openid:  ~p", [ExitY]),
			{false, ReqData, Session}
	end.

to_html(ReqData, Session) ->
	?info("to html"),
	{ReqData0,Session0} = case wrq:path_info(action, ReqData) of
		"login" ->
			{ReqData,Session};
		"logout" ->
			SessionId = rpgb_session:get_id(Session),
			rpgb_session:destroy(SessionId),
			{ReqData, undefined}
	end,
	{ok, Out} = base_dtl:render([{session, rpgb_session:to_dict(Session0)}]),
	{Out, ReqData0, Session0}.

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
