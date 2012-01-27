-module(rpgb_templates).

-export([init/1, to_html/2]).
-include_lib("webmachine/include/webmachine.hrl").

init([Page]) ->
	{ok, Page}.

to_html(Req, index) ->
	{ok, Out} = base_dtl:render([]),
	{Out, Req, index};

to_html(Req, Ctx) ->
	{<<>>, Req, Ctx}.
