-module(rpgb_files).

-export([init/1,to_html/2]).
-include_lib("webmachine/include/webmachine.hrl").


init(Args) ->
	{ok, undefined}.

to_html(ReqData, Context) ->
	Path = case wrq:path(ReqData) of
		"/" -> "index.html";
		X -> X
	end,
	{ok, Bin} = file:read_file("priv/www/" ++ Path),
	{Bin, ReqData, Context}.
