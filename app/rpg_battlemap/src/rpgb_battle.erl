-module(rpgb_battle).

-behavior(gen_event).

-include("log.hrl").
-include("rpg_battlemap.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% public api
-export([join/1, leave/1]).

% gen_event
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
	code_change/3]).

-record(state,{
	consumers = [],
	map_id,
	layers = [],
	combatants = [],
	zones = []
}).

join(MapId) ->
	?debug("bing"),
	Consumer = self(),
	?debug("bing"),
	case gen_event:call(rpgb_data_events, {?MODULE, MapId}, {add_consumer, Consumer}, infinity) of
		{error, bad_module} ->
			?debug("new subscription"),
			rpgb_data_events:subscribe({?MODULE, MapId}, {MapId, [Consumer]});
		Reply ->
			?debug("exisintant subscription: ~p", [Reply]),
			Reply
	end.

leave(MapId) ->
	Consumer = self(),
	gen_event:call(rpgb_data_events, {?MODULE, MapId}, {drop_consumer, Consumer}),
	ok.

% init
init({MapId, InitialConsumers}) ->
	?debug("battle started for map ~p with initial ~p", [MapId, InitialConsumers]),
	{ok, LayerRecs} = rpgb_data:search(rpgb_rec_layer, [{battlemap_id, MapId}]),
	Layers = [L#rpgb_rec_layer.id || L <- LayerRecs],
	{ok, CombatantRecs} = rpgb_data:search(rpgb_rec_combatant, [{battlemap_id, MapId}]),
	Combatants = [C#rpgb_rec_combatant.id || C <- CombatantRecs],
	Zones = lists:foldl(fun(LayerId, Acc) ->
		{ok, ZoneRecs} = rpgb_data:search(rpgb_rec_zone, [{layer_id, LayerId}]),
		ZoneIds = [Z#rpgb_rec_zone.id || Z <- ZoneRecs],
		Acc ++ ZoneIds
	end, [], Layers),
	State = #state{
		map_id = MapId, consumers = ordsets:from_list(InitialConsumers),
		layers = Layers,
		combatants = Combatants,
		zones = Zones},
	{ok, State}.

% handle_event
handle_event({delete, rpgb_rec_battlemap, MapId} = Msg, #state{map_id = MapId} = State) ->
	tell_consumers(State#state.consumers, Msg),
	remove_handler;

handle_event({delete, rpgb_rec_layer, LayerId} = Msg, State) ->
	Layers = State#state.layers,
	case lists:delete(LayerId, Layers) of
		Layers ->
			{ok, State};
		Layers1 ->
			tell_consumers(State#state.consumers, Msg),
			{ok, State#state{layers = Layers1}}
	end;

handle_event({update, #rpgb_rec_battlemap{id = MapId}} = Msg, #state{map_id = MapId} = State) ->
	tell_consumers(State#state.consumers, Msg),
	{ok, State};

%handle_event({new, Map}, State) when is_record(Map, rpgb_rec_battlemap) ->
%	{ok, State}.

handle_event(Event, State) ->
	?debug("ignored event ~p", [Event]),
	{ok, State}.

% handle_call
handle_call({add_consumer, Consumer}, State) ->
	Consumers = ordsets:add_element(Consumer, State#state.consumers),
	erlang:monitor(process, Consumer),
	{ok, ok, State#state{consumers = Consumers}};

handle_call({drop_consumer, Consumer}, State) ->
	case ordsets:del_element(Consumer, State#state.consumers) of
		[] ->
			{remove_handler, ok};
		Consumers ->
			{ok, ok, State#state{consumers = Consumers}}
	end.

% handle_info
handle_info({'DOWN', _Mon, process, Pid, _Why}, State) ->
	case ordsets:del_element(Pid, State#state.consumers) of
		[] ->
			remove_handler;
		Consumers ->
			{ok, State#state{consumers = Consumers}}
	end;

handle_info(Msg, State) ->
	?debug("Some info: ~p", [Msg]),
	{ok, State}.

% terminate
terminate(Why, State) ->
	tell_consumers(State#state.consumers, {exit, Why}),
	ok.

% code_change
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% internal functions

tell_consumers(Pids, Msg) ->
	?debug("Telling ~p", [Pids]),
	[Pid ! {map_event, Msg} || Pid <- Pids].

-ifdef(TEST).

use_test_() ->
	{setup, local, fun() ->
		rpgb_test_util:stop_data(),
		meck:new(rpgb_battle_data),
		rpgb_data:start_link(rpgb_battle_data),
		rpgb_data_events:start_link()
	end,
	fun(_) ->
		meck:unload(rpgb_battle_data),
		rpgb_test_util:stop_data(),
		rpgb_data_events:stop()
	end,
	fun(_) -> [

		{"subscribe works", fun() ->
			meck:expect(rpgb_battle_data, search, fun(_, _) -> ?debugMsg("bing"), {ok, []} end),
			Got = ?MODULE:join(1),
			?assertEqual(ok, Got)
		end},

		{"gets a map event", fun() ->
			meck:expect(rpgb_battle_data, save, fun(Map) ->
				{ok, Map}
			end),
			Map = #rpgb_rec_battlemap{id = 1},
			rpgb_data:save(Map),
			MapGot = receive
				{map_event, {update, Map}} ->
					Map
			after 1000 ->
				{error, timeout}
			end,
			?assertEqual(Map, MapGot)
		end}

	] end}.

-endif.