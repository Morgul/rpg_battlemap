-module(rpgb_handle_map_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-include_lib("triq/include/triq_statem.hrl").

% both eunit and triq define let, triq lets (heh) eunit win
% so, setting up a triqlet
-define(TRIQLET(X,Gen1,Gen2), 
	?DOMAIN_MODULE:bind(Gen1, fun(X)->Gen2 end)).

-define(cookie, {"Cookie", element(2, cowboy_cookies:cookie(<<"rpgbsid">>, <<"sessionid">>))}).
-define(accepts, {"Accepts", "application/json"}).

-compile(export_all).

-record(state, {map_id}).

property_test_() -> {timeout, 60000, ?assert(check())}.

prop_map_statem() ->
	application:start(cowboy),
	HostPort = {<<"localhost">>, 9093},
	cowboy:start_listener(handle_map_tests, 1,
		cowboy_tcp_transport, [{port, 9093}],
		cowboy_http_protocol, [{dispatch, [
			{'_', [
				{[<<"map">>], rpgb_handle_map, HostPort},
				{[<<"map">>, mapid], rpgb_handle_map, HostPort}
			]}
		]}]
	),
	ibrowse:start(),
	rpgb_test_util:mecked_data(handle_map_data),
	rpgb_session:make_ets(),
	{ok, Session} = rpgb_session:get_or_create(<<"id">>),
	Session1 = setelement(1, Session, <<"sessionid">>),
	ets:insert(rpgb_session, Session1),
	?FORALL(Cmds, triq_statem:commands(?MODULE), begin
		triq_statem:run_commands(?MODULE, Cmds),
		true
	end).

initial_state() ->
	#state{}.

command(State) ->
	frequency([
		{1, {call, ?MODULE, create_map, [g_mapjson(), State]}},
		{1, {call, ?MODULE, destroy_map, [State]}},
		{5, {call, ?MODULE, update_map, [g_mapjson(), State]}}
	]).

%% =======================================================
%% generators
%% =======================================================

g_mapjson() ->
	?TRIQLET(X, list(oneof([
		{name, g_name()},
		{background_color,  g_color()},
		{gridline_color,  g_color()},
		{grid_opacity,  g_opacity()}
	])), uniquify(X)).

g_name() ->
	?TRIQLET(N,
	list(
		frequency([
			{8, char()},
			{2, oneof([$ , $1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $', $"])}
		])
	), list_to_binary(N)).

g_color() ->
	oneof([
		<<"black">>, <<"blue">>, <<"green">>, g_color_rgb(), g_color_rgba()
	]).

g_opacity() ->
	?TRIQLET(N, int(), case N of 0 -> 0; _ -> 1 / abs(N) end).

g_color_rgb() ->
	[R,B,G,_] = g_color_rgba(),
	[R,B,G].

g_color_rgba() ->
	[g_256(), g_256(), g_256(), g_opacity()].

g_256() ->
	choose(0, 255).

uniquify(X) ->
	uniquify(X, []).

uniquify([], Acc) ->
	Acc;

uniquify([{K, V} | Tail], Acc) ->
	Acc1 = orddict:store(K, V, Acc),
	uniquify(Tail, Acc1).

%% =======================================================
%% preconditions
%% =======================================================

precondition(#state{map_id = undefined}, {call, _, create_map, _}) ->
	true;
precondition(#state{map_id = undefined}, Blorp) ->
	false.

%% =======================================================
%% next_state
%% =======================================================

next_state(State, {ok, _Status, _Heads, Body}, {call, _, create_map, _}) ->
	Props = jsx:to_term(Body),
	Id = proplists:get_value(<<"url">>, Props),
	State#state{map_id = Id};

next_state(State, {ok, _Status, _Heads, _Body}, {call, _, destroy_map, _}) ->
	State#state{map_id = undefined};

next_state(State, _Res, _Call) ->
	State.

%% =======================================================
%% tests proper
%% =======================================================

create_map(Json, #state{map_id = undefined}) ->
	Url = "http://localhost:9093/map",
	Binary = jsx:to_json(Json),
	ibrowse:send_req(Url, [?cookie, ?accepts], put, Binary).

destroy_map(#state{map_id = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts], delete).

update_map(Json, #state{map_id = Url}) ->
	ibrowse:send_req(Url, [?cookie, ?accepts], put, jsx:to_json(Json)).

%% =======================================================
%% postcondition
%% =======================================================

postcondition(State, {call, _, create_map, [Json, InState]}, {ok, "204", Heads, Body}) ->
	assert_body(Json, Body);

postcondition(State, {call, _, update_map, [Json, InState]}, {ok, "200", Heads, Body}) ->
	assert_body(Json, Body);

postcondition(State, {call, _, destroy_map, [_InState]}, {ok, "204", Heads, Body}) ->
	true;

postcondition(State, Call, Res) ->
	?debugFmt("State:  ~p\nCall:  ~p\nRes:  ~p", [State, Call, Res]),
	false.

assert_body(Json, Body) ->
	Json1 = binary_keys(Json),
	BodyJson = jsx:to_term(Body),
	match_keys(lists:sort(Json1), lists:sort(BodyJson)).

binary_keys(L) ->
	binary_keys(L, []).

binary_keys([], Acc) ->
	lists:reverse(Acc);

binary_keys([{K, V} | Tail], Acc) when is_atom(K) ->
	binary_keys(Tail, [{list_to_binary(atom_to_list(K)), V} | Acc]);

binary_keys([{K, _} = H | Tail], Acc) when is_binary(K) ->
	binary_keys(Tail, [H | Acc]).

match_keys([], _) ->
	true;
match_keys(_, []) ->
	false;
match_keys([{Key, Val} | ETail], [{Key, Val} | GTail]) ->
	match_keys(ETail, GTail);
match_keys(Expected, [Got | Tail]) ->
	match_keys(Expected, Tail).
