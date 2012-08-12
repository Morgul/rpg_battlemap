-module(rpgb_util).
-export([hexstr_to_bin/1, bin_to_hexstr/1]).
% functions to help set up testing
-export([start_testnode/0, start_testnode/1, start_testnode/2, add_paths/0,
	cover_mods/1]).

%% @doc Converts binaries to hexidecimal strings.
-spec(bin_to_hexstr/1 :: (Bin :: binary()) -> [48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 97 | 98 | 99 | 100 | 101 | 102]).
bin_to_hexstr(Bin) ->
	string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)])).

%% @doc Converts a hexidecimal string in any case to a binary.
-spec(hexstr_to_bin/1 :: (S :: string()) -> binary() | 'error').
hexstr_to_bin(S) ->
	hexstr_to_bin(string:to_lower(S), []).

-define(HEX, [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f]).

%% @private
-spec(hexstr_to_bin/2 :: (string(), Acc :: string()) -> binary() | 'error').
hexstr_to_bin([], Acc) ->
	list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X, Y | T], Acc) ->
	case {lists:member(X, ?HEX), lists:member(Y, ?HEX)} of
		{true, true} ->
			{ok, [V], []} = io_lib:fread("~16u", [X, Y]),
			hexstr_to_bin(T, [V | Acc]);
		_Else ->
			error
	end.

start_testnode() ->
	add_paths(),
	case node() of
		nonode@nohost ->
			[] = os:cmd("epmd -daemon"),
			case net_kernel:start([rpg_battlemap_test, shortnames]) of
				{ok, _} ->
					node();
				{error, {{already_started, _}, _}}  ->
					node();
				_ ->
					erlang:error(node_fail)
			end;
		Else ->
			Else
	end.

start_testnode(Name) ->
	start_testnode(Name, net_adm:localhost()).

start_testnode(Name, Host) ->
	start_testnode(),
	case slave:start_link(Host, Name) of
		{ok, N} -> 
			rpc:call(N, ?MODULE, add_paths, []),
			Covered = [Mod || {Mod, cover_compiled} <- code:all_loaded()],
			rpc:call(N, ?MODULE, cover_mods, [Covered]),
			N;
		{error, {already_running, N}} -> N
	end.

add_paths() ->
	{Pre, Deps} = case {file:list_dir("deps"), file:list_dir("../deps")} of
		{{ok, Files}, _} -> {"deps/", Files};
		{_, {ok, Files}} -> {"../deps/", Files};
		_ -> {[], []}
	end,
	Paths = [Pre ++ X ++ "/ebin" || X <- Deps],
	code:add_paths(Paths).

cover_mods([]) ->
	cover:start();
cover_mods([Mod | Tail]) ->
	{ok, Mod} = cover:compile_beam(Mod),
	cover_mods(Tail).
