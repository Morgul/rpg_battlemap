#! /usr/bin/env escript
%%! -sname rpg_battlemap_dev
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
	ModelFiles = filelib:wildcard("./models/*.erl"),
	compile_models(ModelFiles).

compile_models([]) -> ok;
compile_models([ModelFile | Tail]) ->
	ModelName = filename:rootname(ModelFile),
	ModelLastMod = filelib:last_modified(ModelFile),
	BeamFile = filename:join(["ebin", ModelName ++ ".beam"]),
	BeamLastMod = filelib:last_modified(BeamFile),
	Res = if
		ModelLastMod > BeamLastMod ->
			boss_record_compiler:compile(ModelFile, [{out_dir, "ebin"}]);
		true ->	
			{ok, nochange}
	end,
	case Res of
		{ok, _} ->
			compile_models(Tail);
		{error, _} = Err ->
			io:format("~p", [Err]),
			halt(1)
	end.

build_db() ->
	mnesia:create_schema([node()]),
	io:format("Schema created\n"),
	mnesia:start(),
	% yeah hard coded stuff!
	TableData = [

		% boss_db needs this to generate ids.
		{'_ids_', [{disc_copies, [node()]}]},

		% prefixing rpgb_ on table names because module 'group' is used.
		{rpgb_user, [
			{attributes,
				[id, name, open_id, rpgb_group_id, created_time, updated_time]},
			{disc_copies, [node()]}
		]},

		{rpgb_group, [
			{attributes, [id, name, created_time, updated_time]},
			{disc_copies, [node()]}
		]},

		{rpgb_permission, [
			{attributes, [id, tag, rpgb_user_id, rpgb_group_id]},
			{disc_copies, [node()]}
		]}

	],
	[io:format("Creating table ~s:  ~p\n", [Tname, mnesia:create_table(Tname,
		Attr)]) || {Tname, Attr} <- TableData].
