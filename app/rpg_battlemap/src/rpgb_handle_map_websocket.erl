-module(rpgb_handle_map_websocket).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([init/4, stream/3, info/3, terminate/2]).

-record(ctx, {active, hostport}).

init(_Transport, Req, Opts, Active) ->
	Hostport = proplists:get_value(hostport, Opts),
	{ok, Req, #ctx{active = Active, hostport = Hostport}}.

stream(Data, Req, Ctx) ->
	?info("Got data:  ~p", [Data]),
	{reply, Data, Req, Ctx}.

info(Info, Req, Ctx) ->
	?info("got info:  ~p", [Info]),
	{ok, Req, Ctx}.

terminate(Req, Ctx) ->
	?info("termination"),
	ok.
