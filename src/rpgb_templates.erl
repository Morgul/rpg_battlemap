-module(rpgb_templates).

-export([init/1, to_html/2]).
-include_lib("webmachine/include/webmachine.hrl").

init([Page]) ->
	{ok, Page}.

to_html(Req, index) ->
	{ok, Session, Req0} = rpgb_session:get_or_create(Req),
	{ok, Out} = base_dtl:render([{"session", rpgb_session:to_dict(Session)}]),
	{Out, Req0, index};

to_html(Req, Ctx) ->
	{<<>>, Req, Ctx}.
