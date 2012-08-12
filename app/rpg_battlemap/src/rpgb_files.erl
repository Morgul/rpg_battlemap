-module(rpgb_files).

-export([init/1,to_html/2,resource_exists/2]).
-include_lib("webmachine/include/webmachine.hrl").
-include("log.hrl").


init(_Args) ->
	{ok, undefined}.

resource_exists(ReqData, _Context) ->
	Path = case wrq:path(ReqData) of
		"/" -> "index.html";
		[$/ | X] -> X;
		X -> X
	end,
	Priv = code:priv_dir(rpg_battlemap),
	Filename = filename:join([Priv, "www", Path]),
	{filelib:is_file(Filename), ReqData, Filename}.

to_html(ReqData, Path) ->
	Ext = filename:extension(Path),
	Mime = mochiweb_mime:from_extension(Ext),
	{ok, Bin} = file:read_file(Path),
	ReqData0 = wrq:set_resp_header("Content-Type", Mime, ReqData),
	{Bin, ReqData0, Path}.
