-module(rpgb_handle_map_websocket).
-behavior(cowboy_websocket_handler).

-include("rpg_battlemap.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-else.
-define(debugFmt(_Fmt,_Args), ok).
-define(debugMsg(_Msg), ok).
-endif.

-export([get_routes/0]).
-export([init/3, websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

-record(state, {user, session, map}).

get_routes() ->
	[<<"/map/:mapid/ws">>].

init(_Protocol, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opt) ->
	BindRes = rpgb:bind({Req, #state{}}, [
		fun get_session/1,
		fun get_user/1,
		fun map_id_is_integer/1,
		fun map_exists/1,
		fun user_owns_map/1
	]),
	case BindRes of
		{ok, {Req1, State}} ->
			{ok, Req1, State};
		{error, Req2} ->
			{shutdown, Req2}
	end.

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, <<"okie: ", Msg/binary>>}, Req, State};
websocket_handle(Msg, Req, State) ->
	{ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info(Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
	ok.

get_session({Req, State}) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	State2 = #state{session = Session},
	{ok, {Req1, State2}}.

get_user({Req, State}) ->
	case rpgb_session:get_user(State#state.session) of
		undefined ->
			{ok, Req1} = cowboy_req:reply(401, Req),
			{error, Req1};
		User ->
			{ok, {Req, State#state{user = User}}}
	end.

get_raw_mapid({Req, State}) ->
	case cowboy_req:binding(mapid, Req) of
		{undefined, Req2} ->
			{ok, Req2} = cowboy_req:reply(404, Req),
			{error, Req2};
		{MapId, Req2} ->
			State2 = State#state{map = MapId},
			{ok, {Req2, State2}}
	end.

map_id_is_integer({Req, State}) ->
	{MapId, Req1} = cowboy_req:binding(mapid, Req),
	try list_to_integer(binary_to_list(MapId)) of
		N ->
			State2 = State#state{map = N},
			{ok, {Req, State2}}
	catch
		error:badarg ->
			{ok, Req3} = cowboy_req:reply(404, Req1),
			{error, Req3}
	end.

map_exists({Req, State}) ->
	case rpgb_data:get_by_id(rpgb_rec_battlemap, State#state.map) of
		{error, notfound} ->
			{ok, Req1} = cowboy_req:reply(404, Req),
			{error, Req1};
		{ok, Map} ->
			{ok, {Req, State#state{map = Map}}}
	end.

user_owns_map({Req, State}) ->
	#state{user = User, map = Map} = State,
	if
		User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id ->
			{ok, {Req, State}};
		true ->
			{ok, Req1} = cowboy_req:reply(403, Req),
			{error, Req1}
	end.
