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

allowed_methods(ReqData, _Ctx) ->
	?info("allowed methods"),
	{ok, Session, ReqData0} = rpgb_session:get_or_create(ReqData),
	{['GET', 'POST', 'HEAD'], ReqData0, Session}.

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
	
process_post(ReqData, Session) ->
	?info("processing a likely login"),
	Post = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
	Openid = proplists:get_value("openid", Post, ""),
	?info("Openid:  ~p", [Openid]),
	SessionId = rpgb_session:get_id(Session),
	case gen_server:call(openid, {prepare, SessionId, Openid, true}) of
		{ok, AuthReq} ->
			BaseURL = rpg_battlemap_app:get_url(),
			ReturnTo = <<BaseURL/binary, "/account/login_complete">>,
			AuthURL = openid:authentication_url(AuthReq, ReturnTo, BaseURL, [
				{"openid.sreg.optional", "nickname"}]),
			ReqData1 = wrq:set_resp_header("Location", binary_to_list(AuthURL), ReqData),
			ReqData2 = wrq:do_redirect(true, ReqData1),
			{true, ReqData2, Session};
		{error, Err} ->
			?info("Error from openid:  ~p", [Err]),
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
			{ReqData, undefined};
		"login_complete" ->
			SessionId = rpgb_session:get_id(Session),
			BaseURL = rpg_battlemap_app:get_url(),
			ReturnTo = <<BaseURL/binary, "/account/login_complete">>,
			QueryParams = wrq:req_qs(ReqData),
			case gen_server:call(openid, {verify, SessionId, binary_to_list(ReturnTo), QueryParams}) of
				{ok, OpenID} ->
					io:format("We have ourselves a user:  ~p", [OpenID]),
					io:format("query params:  ~p", [QueryParams]),
					Username = proplists:get_value("openid.sreg.nickname", QueryParams, "Awesome User"),
					case boss_db:find(rpgb_user, [{open_id, equals, list_to_binary(OpenID)}], 1) of
						[] ->
							Userrec = boss_record:new(rpgb_user, [
								{open_id, list_to_binary(OpenID)},
								{name, list_to_binary(Username)},
								{rpgb_ground_id, 'rpgb_ground-1'}
							]),
							io:format("Das user:  ~p\n", [Userrec]),
							{ok, Userrec0} = Userrec:save(),
							{ok, Session1} = rpgb_session:set_user(Userrec0:attributes(), Session),
							{ReqData, Session1};
						[Userrec] ->
							{ok, Session1} = rpgb_session:set_user(Userrec:attributes(), Session),
							{ReqData, Session1}
					end;
				{error, Fail} ->
					io:format("And it's all fucked up:  ~p", [Fail]),
					{ReqData, Session}
			end
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
