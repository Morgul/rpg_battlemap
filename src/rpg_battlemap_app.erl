-module(rpg_battlemap_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% api
-exprt([get_env/1, get_env/2, get_url/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rpg_battlemap_sup:start_link(application:get_all_env(rpg_battlemap)).

stop(_State) ->
    ok.


%% ===================================================================
%% Utilties
%% ===================================================================

get_env(Key) ->
	application:get_env(rpg_battlemap, Key).

get_env(Key, Default) ->
	case application:get_env(Key) of
		undefined -> {ok, Default};
		E -> E
	end.

get_url() ->
	{ok, Host} = get_env(host, "localhost"),
	{ok, Port} = get_env(port, 9090),
	{ok, Proto} = get_env(protocol, http),
	iolist_to_binary(io_lib:format("~s://~s:~p", [Proto,Host, Port])).
