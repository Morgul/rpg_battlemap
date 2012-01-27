-module(rpg_battlemap_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rpg_battlemap_sup:start_link(application:get_all_env(rpg_battlemap)).

stop(_State) ->
    ok.
