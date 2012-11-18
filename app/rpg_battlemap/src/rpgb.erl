-module(rpgb).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([res_init/1, get_env/1, get_env/2, get_url/0,get_url/2,get_url/3,
	get_url/4,sluggify/1, get_routes/2]).
%-export([is_printable/1, is_not_printable/1, is_string/1]).
%-export([to_json/1, to_json/2]).
-export([set_proplist/3, set_proplist/2]).
-export([now_to_timestamp/1]).
-export([refresh_templates/1]).
-export([bind/2]).

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
	get_url(http, Host, Port, []).

get_url(Req, Path) when is_tuple(Req) ->
	Proto = case cowboy_http_req:transport(Req) of
		{ok, cowboy_ssl_transport, _} ->
			https;
		_ ->
			http
	end,
	{ok, Host} = get_env(host, "localhost"),
	{ok, Port} = get_env(port, 9090),
	get_url(Proto, Host, Port, Path);

get_url(Host, Port) ->
	get_url(http, Host, Port, []).

get_url(Host, Port, Path) ->
	get_url(http, Host, Port, Path).

get_url(Proto, Host, Port, Path) when is_list(Path), is_list(hd(Path)) ->
	Path1 = filename:join(Path),
	get_url(Proto, Host, Port, Path1);
get_url(Proto, Host, Port, [$/ | Path]) ->
	get_url(Proto, Host, Port, Path);
get_url(Proto, Host, Port, <<$/, Path/binary>>) ->
	get_url(Proto, Host, Port, Path);
get_url(Req, Host, Port, Path) when is_tuple(Req) ->
	Proto = case cowboy_http_req:transport(Req) of
		{ok, cowboy_ssl_transport, _} ->
			https;
		_ ->
			http
	end,
	get_url(Proto, Host, Port, Path);
get_url("http", Host, 80, Path) ->
	iolist_to_binary(io_lib:format("http://~s/~s", [Host, Path]));
get_url("https", Host, 443, Path) ->
	iolist_to_binary(io_lib:format("https://~s/~s", [Host, Path]));
get_url(Proto, Host, Port, Path) ->
	iolist_to_binary(io_lib:format("~s://~s:~p/~s", [Proto, Host, Port, Path])).

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

get_routes(HP, Mods) ->
	get_routes(HP, Mods, []).

get_routes(_HP, [], Acc) ->
	lists:reverse(Acc);

get_routes(HP, [Mod | Tail], Acc) ->
	Routes = Mod:get_routes(),
	Acc2 = make_route_tuple(HP, Mod, Routes, Acc),
	get_routes(HP, Tail, Acc2).

make_route_tuple(_HP, _Mod, [], Acc) ->
	Acc;

make_route_tuple(HP, Mod, [{Route, Opts} | Tail], Acc) ->
	Tuple = {Route, Mod, [HP | Opts]},
	make_route_tuple(HP, Mod, Tail, [Tuple | Acc]);
make_route_tuple(HP, Mod, [Route | Tail], Acc) ->
	Tuple = {Route, Mod, HP},
	make_route_tuple(HP, Mod, Tail, [Tuple | Acc]).

refresh_templates(Template) when is_atom(Template) ->
	refresh_templates([Template]);

refresh_templates(Templates) ->
	LibDir = code:lib_dir(rpg_battlemap, templates),
	case filelib:is_dir(LibDir) of
		true ->
			refresh_templates(Templates, LibDir);
		_ ->
			ok
	end.

refresh_templates([], _) ->
	ok;

refresh_templates([Template | Tail], LibDir) ->
	"ltd_" ++ RevFile = lists:reverse(atom_to_list(Template)),
	File = lists:reverse(RevFile) ++ ".html",
	erlydtl:compile(LibDir ++ "/" ++ File, Template, [{out_dir, code:lib_dir(rpg_battlemap, ebin)}]),
	code:soft_purge(Template),
	{module, Template} = code:load_file(Template),
	refresh_templates(Tail, LibDir).

bind(Arg, []) ->
	{ok, Arg};

bind(Arg, [Fun | Tail]) ->
	case Fun(Arg) of
		{ok, Arg2} ->
			bind(Arg2, Tail);
		Else ->
			Else
	end.

-ifdef(TEST).

bind_test_() -> [
	{"three successes", fun() ->
		F1 = fun(a) ->
			{ok, b}
		end,
		F2 = fun(b) ->
			{ok, c}
		end,
		F3 = fun(c) ->
			{ok, d}
		end,
		?assertEqual({ok, d}, bind(a, [F1, F2, F3]))
	end},

	{"boom on second", fun() ->
		F1 = fun(a) ->
			{ok, b}
		end,
		F2 = fun(b) ->
			{error, b}
		end,
		F3 = fun(c) ->
			{ok, d}
		end,
		?assertEqual({error, b}, bind(a, [F1, F2, F3]))
	end}].

-endif.
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
