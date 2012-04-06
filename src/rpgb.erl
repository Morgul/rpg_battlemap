-module(rpgb).
-export([res_init/1, get_env/1, get_env/2, get_url/0,get_url/1,sluggify/1]).

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

get_url([]) -> get_url();

get_url([I | _] = Path) when is_integer(I) ->
	get_url([Path]);

get_url(Path) ->
	Path0 = string:join(Path,"/"),
	Base = get_url(),
	iolist_to_binary(io_lib:format("~s/~s",[Base,Path0])).

sluggify(Binary) when is_binary(Binary) ->
	list_to_binary(sluggify(binary_to_list(Binary)));
sluggify(String) ->
	{ok, CleanInvalid} = re:compile("[^-a-zA-Z0-9,&\s]+", [caseless]),
	{ok, DashToUnder} = re:compile("-"),
	{ok, SpaceToDash} = re:compile("\\s"),
	Regs = [{CleanInvalid,""},{DashToUnder,"_"},{SpaceToDash,"-"}],
	lists:foldl(fun({Req,Rep},S) -> re:replace(S,Req,Rep) end,String,Regs).
