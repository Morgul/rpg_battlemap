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
	[<<"/maps/:mapid/ws">>].

init(_Protocol, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opt) ->
	BindRes = rpgb:bind({Req, #state{}}, [
		fun get_session/1,
		fun get_user/1,
		fun map_id_is_integer/1,
		fun map_exists/1,
		fun user_is_participant/1,
		fun battle_joined/1
	]),
	case BindRes of
		{ok, {Req1, State}} ->
			{ok, Req1, State};
		{error, Req2} ->
			{shutdown, Req2}
	end.

websocket_handle({text, Msg}, Req, State) ->
	case jsx:to_term(Msg) of
		[{}] ->
			?debugMsg("empty json means no work done"),
			{ok, Req, State};
		BadJson when not is_list(BadJson) ->
			?debugFmt("bad json decode: ~p", [BadJson]),
			{ok, Req, State};
		Json ->
			Action = proplists:get_value(<<"action">>, Json),
			Type = proplists:get_value(<<"type">>, Json),
			Id = proplists:get_value(<<"id">>, Json),
			Data = proplists:get_value(<<"data">>, Json),
			From = proplists:get_value(<<"reply_with">>, Json),
			dispatch(Req, State, From, Action, Type, Id, Data)
	end;
websocket_handle(Msg, Req, State) ->
	{ok, Req, State}.

websocket_info({map_event, {update, Record}}, Req, State) ->
	?debugMsg("map update event"),
	Frame = make_frame(element(1, Record), element(2, Record), <<"put">>, undefined, Record:make_json()),
	{reply, {text, jsx:to_json(Frame)}, Req, State};

websocket_info({map_event, {new, Record}}, Req, State) ->
	?debugMsg("map new event"),
	Frame = make_frame(element(1, Record), element(2, Record), <<"put">>, undefined, Record:make_json()),
	{reply, {text, jsx:to_json(Frame)}, Req, State};

websocket_info({map_event, {delete, Type, Id}}, Req, State) ->
	?debugMsg("map delete event"),
	Frame = make_frame(Type, Id, <<"delete">>, undefined, undefined),
	{reply, {text, jsx:to_json(Frame)}, Req, State};

websocket_info({send, Msg}, Req, State) ->
	?debugFmt("some random send: ~p", [Msg]),
	{reply, {text, Msg}, Req, State};
websocket_info(Info, Req, State) ->
	?debugFmt("Some other info: ~p", [Info]),
	{ok, Req, State}.

websocket_terminate(Reason, _Req, State) ->
	rpgb_battle:leave(State#state.map#rpgb_rec_battlemap.id),
	ok.

battle_joined({Req, State}) ->
	case rpgb_battle:join(State#state.map#rpgb_rec_battlemap.id) of
		ok ->
			{ok, {Req, State}};
		Error ->
			?debugFmt("Battle could not be joined: ~p", [Error]),
			{ok, Req1} = cowboy_req:reply(500, Req),
			{error, {Req1, State}}
	end.

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

user_is_participant({Req, State}) ->
	#state{user = User, map = Map} = State,
	?debugFmt("Checking if user ~p is participant of map ~p", [User#rpgb_rec_user.id, Map]),
	case rpgb_rec_battlemap:is_user_participant(User, Map) of
		true ->
			{ok, {Req, State}};
		false ->
			{ok, Req1} = cowboy_req:reply(403, Req),
			{error, Req1}
	end.

make_reply(From, Accepted, Data) ->
	Json = make_frame(<<"reply">>, From, <<"reply">>, Accepted, Data),
	jsx:to_json(Json).

make_frame(Type, Id, Action, Accepted, Data) ->
	Json1 = maybe_add_type(Type, []),
	Json2 = maybe_add_id(Id, Json1),
	Json3 = maybe_add_accepted(Accepted, Json2),
	Json4 = maybe_add_data(Data, Json3),
	[{<<"action">>, Action} | Json4].

maybe_add_type(rpgb_rec_battlemap, Json) ->
	[{<<"type">>, <<"map">>} | Json];
maybe_add_type(Type, Json) when is_binary(Type) ->
	[{<<"type">>, Type} | Json];
maybe_add_type(Huh, Json) ->
	?debugFmt("I don't understand the type ~p", [Huh]),
	Json.

maybe_add_id(undefined, Json) ->
	Json;
maybe_add_id(Id, Json) ->
	[{<<"type_id">>, Id} | Json].

maybe_add_accepted(undefined, Json) ->
	Json;
maybe_add_accepted(Bool, Json) ->
	[{<<"accepted">>, Bool} | Json].

maybe_add_data(undefined, Json) ->
	Json;
maybe_add_data(Data, Json) ->
	[{<<"data">>, Data} | Json].

dispatch(Req, State, From, <<"put">>, Type, Id, undefined) ->
	?debugMsg("no data put"),
	Reply = make_reply(From, false, <<"no data">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, #state{map = #rpgb_rec_battlemap{owner_id = Owner}, user = #rpgb_rec_user{id = Owner}} = State, From, <<"put">>, <<"map">>, Id, Json) ->
	case rpgb_rec_battlemap:update_from_json(Json, State#state.map) of
		{ok, Map2} ->
			{ok, Map3} = rpgb_data:save(Map2),
			ReplyFrame = make_reply(From, true, rpgb_rec_battlemap:make_json(Map3)),
			?debugFmt("All the world is good updating map: ~p", [ReplyFrame]),
			{reply, {text, ReplyFrame}, Req, State#state{map = Map3}};
		{error, {Wut, English}} ->
			?debugFmt("makeing a reply due to bad update: ~p:~p", [Wut, English]),
			ReplyFrame = make_reply(From, false, English),
			{reply, {text, ReplyFrame}, Req, State}
	end;

dispatch(Req, State, From, <<"put">>, <<"map">>, Id, Json) ->
	?debugMsg("not owner"),
	Reply = make_reply(From, false, <<"not owner">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"get">>, <<"map">>, _Id, _Json) ->
	?debugMsg("basic get"),
	case rpgb_data:get_by_id(rpgb_rec_battlemap, State#state.map#rpgb_rec_battlemap.id) of
		{ok, Map} ->
			Reply = make_reply(From, true, rpgb_rec_battlemap:make_json(Map)),
			{reply, {text, Reply}, Req, State#state{map = Map}};
		{error, notfound} ->
			Reply = make_reply(From, false, <<"map not found">>),
			{reply, {text, Reply}, Req, State}
	end;

dispatch(Req, State, From, <<"delete">>, <<"map">>, _Id, _Json) ->
	?debugMsg("can't do a delete"),
	Reply = make_reply(From, false, undefined),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, undefined, _Action, _Type, _Id, _Data) ->
	?debugMsg("no reply id given"),
	{ok, Req, State};

dispatch(Req, State, From, Action, Type, Id, Data) ->
	?debugFmt("no reply, just not gonna do anything~n"
		"    State: ~p~n"
		"    From: ~p~n"
		"    Action: ~p~n"
		"    Type: ~p~n"
		"    Id: ~p~n"
		"    Data: ~p", [State, From, Action, Type, Id, Data]),
	Reply = make_reply(From, false, <<"nyi">>),
	{reply, {text, Reply}, Req, State}.
