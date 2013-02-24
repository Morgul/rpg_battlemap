-module(rpg_battlemap_sup).

-behaviour(supervisor).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
		{ok, Keyfile} = rpgb:get_env(keyfile, code:priv_dir(rpg_battlemap) ++ "/key"),
		{ok, Certfile} = rpgb:get_env(certfile, code:priv_dir(rpg_battlemap) ++ "/rpgb.crt"),
		Routes = rpgb:get_routes(HP, [rpgb_handle_map, rpgb_handle_user,
			rpgb_handle_layer, rpgb_handle_zone, rpgb_handle_character,
			rpgb_handle_combatant, rpgb_handle_account, rpgb_handle_index,
			rpgb_handle_default]),
		Dispatch = [
			{ListenHost, Routes}
		],
		Dispatch2 = cowboy_router:compile(Dispatch),

		cowboy:start_https(rpgb_listener, Listeners, [{port, Port}, {keyfile, Keyfile}, {certfile, Certfile}], [{env, [{dispatch, Dispatch2}]}]),

    Session = {rpgb_session, {rpgb_session, start_link, []}, permanent,
        5000, worker, [rpgb_session]},

    DataSetup = proplists:get_value(data_callback, Args),
    Data = {rpgb_data, {rpgb_data, start_link, [DataSetup]}, permanent,
        5000, worker, [rpgb_data]},

    OtherModules = proplists:get_value(additional_modules, Args, []),
    OtherModules1 = [{OmId, {OmMod, OmFunc, OmArgs}, permanent, 5000, worker, OmMods} || {OmId, OmMod, OmFunc, OmArgs, OmMods} <- OtherModules],

    Kids = [Session, Data | OtherModules1],

    {ok, { {one_for_one, 5, 10}, Kids} }.

%% ===================================================================
%% Internal
%% ===================================================================

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

start_test() ->
	ssl:start(),
	application:start(ranch),
	application:start(cowboy),
	Out = ?MODULE:start_link([{data_callback, nomod}]),
	?assertMatch({ok, _Pid}, Out),
	{ok, P} = Out,
	unlink(P),
	exit(P, kill).

-endif.
