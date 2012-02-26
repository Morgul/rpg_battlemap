#! /usr/bin/env escript
%%! -sname rpb_battlemap
-mode(compile).

%% @doc Create the mnesia tables, or compile the boss_db models.  Update
%% the node name in the %%! line for deployment.

main(Args) ->
	{ok, DepsDirs} = file:list_dir("deps"),
	Paths = ["deps/" ++ X ++ "/ebin" || X <- DepsDirs],
	code:add_paths(Paths),
	do_actions(Args).

do_actions([]) ->
	halt(0);

do_actions(["compile" | Tail]) ->
	compile_models(),
	do_actions(Tail);

do_actions(["build_db" | Tail]) ->
	build_db(),
	do_actions(Tail).

compile_models() ->
	{ok, ModelFiles} = file:list_dir("models"),
	compile_models(ModelFiles).

compile_models([]) -> ok;
compile_models([ModelFile | Tail]) ->
	File = filename:join(["models", ModelFile]),
	Res = boss_record_compiler:compile(File, [{out_dir, "ebin"}]),
	io:format("Compiling ~s:  ~p\n", [ModelFile, Res]),
	compile_models(Tail).

build_db() ->
	mnesia:start(),
	mnesia:create_schema(),
	% yeah hard coded stuff!
	TableData = [
		% boss_db needs this to generate ids.
		{'_ids_', []},
		% prefixing rpgb_ on table names because module 'group' is used.
		{rpgb_user, [{attributes, [id, name, open_id, group_id, created_time, 
			updated_time]}]},
		{rpgb_group, [{attributes, [id, name, created_time, updated_time]}]},
		{rpgb_permission, [{attributes, [id, tag, user_id, group_id]}]}
	],
	[mnesia:create_table(Tname, [{attributes, Attr}]) ||
		{Tname, Attr} <- TableData].
