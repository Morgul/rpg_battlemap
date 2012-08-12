-module(rpgb).
-export([res_init/1, get_env/1, get_env/2, get_url/0,get_url/1,sluggify/1]).
%-export([is_printable/1, is_not_printable/1, is_string/1]).
%-export([to_json/1, to_json/2]).
-export([set_proplist/3, set_proplist/2]).
-export([now_to_timestamp/1]).

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

set_proplist(Key, Val, Proplist) ->
	set_proplist([{Key, Val}], Proplist).

set_proplist(New, Old) ->
	New0 = [pl_expand(X) || X <- New],
	Old0 = [pl_expand(X) || X <- Old],
	New1 = lists:keysort(1, New0),
	Old1 = lists:keysort(1, Old0),
	Merged = lists:ukeymerge(1, New1, Old1),
	Merged0 = [M || {_K, V} = M <- Merged, V /= undefined],
	proplists:compact(Merged0).

pl_expand(Proplist) ->
	pl_expand(Proplist, []).

pl_expand([], Acc) ->
	lists:reverse(Acc);

pl_expand([{_K,_V} = H | Tail], Acc) ->
	pl_expand(Tail, [H | Acc]);

pl_expand([Atom | Tail], Acc) ->
	pl_expand(Tail, [{Atom, true} | Acc]).

now_to_timestamp({Mega, Sec, Micro}) ->
	% might not be fast, but it's easy.
	[Mega0, Sec0, Micro0] = [integer_to_list(X) || X <- [Mega, Sec, Micro]],
	Sec1 = lists:append(Mega0, Sec0, ".", Micro0),
	Sec2 = list_to_float(Sec1),
	Sec2 * 1000.

%is_string(List) ->
%	lists:any(fun is_not_printable/1, List).
%
%is_printable(X) when is_integer(X) ->
%	if
%		X < 0 -> false;
%		X > 255 -> false;
%		true -> true
%	end;
%is_printable(_X) ->
%	false.
%
%is_not_printable(X) ->
%	not is_printable(X).
%
%to_json(Props) ->
%	to_json(Props, [deep, skip]).
%
%to_json(Props, Options) ->
%	to_json(Props, Options, []).
%
%to_json([], _Options, Acc) ->
%	{struct, Acc};
%
%to_json(Tuple, Options) when is_tuple(Tuple), is_atom(element(1, Tuple)) ->
%	if
%		not erlang:function_exported(element(1), id, 1) and proplists:get_value(skip, Options) ->
%			to_json(