-module(rpgb_files).

-export([init/1,to_html/2,resource_exists/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("log.hrl").


init(Args) ->
	{ok, undefined}.

resource_exists(ReqData, Context) ->
	Path = case wrq:path(ReqData) of
		"/" -> "index.html";
		X -> X
	end,
	{filelib:is_file("priv/www" ++ Path), ReqData, Context}.

to_html(ReqData, Context) ->
	Path = case wrq:path(ReqData) of
		"/" -> "index.html";
		X -> X
	end,
	Ext = filename:extension(Path),
	Mime = mochiweb_mime:from_extension(Ext),
	{ok, Bin} = file:read_file("priv/www" ++ Path),
	ReqData0 = wrq:set_resp_header("Content-Type", Mime, ReqData),
	{Bin, ReqData0, Context}.
