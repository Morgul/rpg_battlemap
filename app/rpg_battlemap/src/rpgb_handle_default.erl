-module(rpgb_handle_default).

-include("log.hrl").

-export([get_routes/0]).
-export([init/3, handle/2, terminate/3]).

get_routes() ->
	[
		<<"/[...]">>
	].

init(_Protos, Req, [{Host, Port}]) ->
	%{upgrade, protocol, cowboy_http_rest},
	{ok, Req, {Host, Port}}.

handle(Req, {Host, Port}) ->
	{Path, Req1} = cowboy_req:path(Req),
	PrivDir = code:priv_dir(rpg_battlemap),
	Path1 = case Path of
		<<"/">> ->
			<<"index.html">>;
		<<$/, Rest/binary>> ->
			Rest;
		_ ->
			Path
	end,
	File = filename:join([PrivDir, "www", Path1]),
	IsFile = filelib:is_file(File),
	if
		IsFile ->
			Ext = filename:extension(File),
			Mime = rpgb_mime:from_ext(Ext),
			{ok, Bin} = file:read_file(File),
			?debug("Serving ~p for ~p", [File, Path]),
			{ok, Req2} = cowboy_req:reply(200, [{<<"Content-Type">>, Mime}], Bin, Req1),
			{ok, Req2, {Host, Port}};
		true ->
			?debug("no file for ~p", [Path]),
			{ok, Req2} = cowboy_req:reply(404, [], <<>>, Req1),
			{ok, Req2, {Host, Port}}
	end.

terminate(_Cause, _Req, _Ctx) ->
	ok.
