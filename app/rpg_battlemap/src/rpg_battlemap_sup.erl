
-module(rpg_battlemap_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
        ssl:start(),
        supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
		{ok, ListenHost} = rpgb:get_env(listen_host, '_'),
		{ok, Host} = rpgb:get_env(hostname, <<"localhost">>),
		{ok, Port} = rpgb:get_env(port, 9090),
		{ok, Listeners} = rpgb:get_env(listeners, 100),
		HP = {Host, Port},
    Dispatch = [
			{ListenHost, [
				{[<<"maps">>], rpgb_handle_maps, HP},
				{[<<"maps">>, map_id], rpgb_handle_map, HP},
				{[<<"maps">>, map_id, property], rpgb_handle_map, {host, Port}},
				{[<<"users">>], rpgb_handle_users, HP},
				{[<<"users">>, user_id], rpgb_handle_user, HP},
				{[<<"users">>, user_id, property], rpgb_handle_user, HP},
				{[<<"layers">>, layer_id], rpgb_handle_layer, HP},
				{[<<"layers">>, layer_id, property], rpgb_handle_layer, HP},
				{[<<"zones">>, zone_id], rpgb_handle_zone, HP},
				{[<<"zones">>, zone_id, property], rpgb_handle_zone, HP},
				{[<<"characters">>], rpgb_handle_characters, HP},
				{[<<"characters">>, id], rpgb_handle_character, HP},
				{[<<"characters">>, id, property], rpgb_handle_character, HP},
				{[<<"combatants">>, id], rpgb_handle_combatants, HP},
				{[<<"combatants">>, id, property], rpgb_handle_combatants, HP},
				{[], rpgb_handle_template, {HP, index_dtl}},
				{'_', rpgb_handle_default, HP}
			]}
		],

		cowboy:start_listener(rpgb_listener, Listeners, 
			cowboy_tcp_transport, [{port, Port}],
			cowboy_http_protocol, [{dispatch, Dispatch}]
		),

    Session = {rpgb_session, {rpgb_session, start_link, []}, permanent,
        5000, worker, [rpgb_session]},

    DataSetup = proplists:get_value(data_callback, Args),
    Data = {rpgb_data, {rpgb_data, start_link, [DataSetup]}, permanent,
        5000, worker, [rpgb_data]},

    OtherModules = proplists:get_value(additional_modules, Args, []),
    OtherModules1 = [{OmId, {OmMod, OmFunc, OmArgs}, permanent, 5000, worker, OmMods} || {OmId, OmMod, OmFunc, OmArgs, OmMods} <- OtherModules],

    Kids = [Session, Data | OtherModules1],

    {ok, { {one_for_one, 5, 10}, Kids} }.
