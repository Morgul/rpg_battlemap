-module(rpgb).
-export([res_init/1, get_env/1, get_env/2, get_url/0]).

res_init(Term) ->
	case get_env(trace) of
		undefined ->
			{ok, Term};
		{ok, TraceDir} ->
			{{trace, TraceDir}, Term}
	end.

get_env(Key) ->
	application:get_env(rpg_battlemap, Key).

get_env(Key, Default) ->
	case application:get_env(rpg_battlemap, Key) of
		undefined -> {ok, Default};
		E -> E
	end.

get_url() ->
	{ok, Host} = get_env(host, "localhost"),
	{ok, Port} = get_env(port, 9090),
	{ok, Proto} = get_env(protocol, http),
	iolist_to_binary(io_lib:format("~s://~s:~p", [Proto,Host, Port])).
