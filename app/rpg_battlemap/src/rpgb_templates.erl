-module(rpgb_templates).

-export([init/1, to_html/2]).

-define(basedirize(X), filename:join("app/rpg_battlemap", X)).

init([Page]) ->
	case rpgb:get_env(trace) of
		{ok, _Tracedir} ->
			{File,Module} = case Page of
				index ->
					{?basedirize("templates/base.html"), base_dtl};
				edit ->
					{?basedirize("templates/editor.html"), editor_dtl};
				battlemap ->
					{?basedirize("templates/battlemap.html"), battlemap_dtl}
			end,
			ok = erlydtl:compile(File, Module, [{out_dir, ?basedirize("ebin")}])
	end,
	{ok, Page}.

to_html(Req, index) ->
	{ok, Session, Req0} = rpgb_session:get_or_create(Req),
	{ok, Out} = base_dtl:render([{"session", rpgb_session:to_dict(Session)}]),
	{Out, Req0, index};

to_html(Req, edit) ->
	{ok, Session, Req0} = rpgb_session:get_or_create(Req),
	{ok, Out} = editor_dtl:render([{"session",rpgb_session:to_dict(Session)}]),
	{Out,Req0,edit};

to_html(Req, Ctx) ->
	{<<>>, Req, Ctx}.
