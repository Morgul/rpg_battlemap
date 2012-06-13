#! /usr/bin/env escript
-mode(compile).

%% @doc Create the mnesia tables, or compile the boss_db models.  Update
%% the node name in the %%! line for deployment.
main(Args) ->
	{ok, DepsDirs} = file:list_dir("deps"),
	Paths = ["deps/" ++ X ++ "/ebin" || X <- DepsDirs],
	code:add_paths(Paths),
	put(out_dir, "ebin"),
	do_actions(Args).

do_actions([]) ->
	halt(0);

do_actions(["compile" | Tail]) ->
	compile_models(),
	do_actions(Tail);

do_actions(["build_db" | Tail]) ->
	{Tail0, DbOpts} = get_db_opts(Tail),
	build_db(DbOpts),
	do_actions(Tail0);

do_actions(["-debug" | Tail]) ->
	put(out_dir, ".eunit"),
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
			boss_record_compiler:compile(ModelFile, [{out_dir, get(out_dir)}]);
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

get_db_opts(Args) ->
	get_db_opts(Args, []).

get_db_opts([], Acc) ->
	{[], Acc};
get_db_opts([[$- | OptName] | Tail], Acc) ->
	case OptName of
		"nodename=" ++ Node ->
			Acchead = {nodename, Node},
			get_db_opts(Tail, [Acchead | Acc]);
		_ ->
			{Tail, Acc}
	end.

get_model_data() ->
	code:add_paths(["ebin"]),
	ModelFiles = filelib:wildcard("./models/*.erl"),
	ModelNames = [filename:rootname(Name) || "./models/" ++ Name <- ModelFiles],
	get_model_data(ModelNames).

get_model_data(Names) ->
	get_model_data(Names, []).

get_model_data([], Acc) ->
	Acc;

get_model_data([Name | Tail], Acc) ->
	Name0 = list_to_atom(Name),
	Rec = boss_record:new(Name0, []),
	Attrs = Rec:attribute_names(),
	Head = {Name0, Attrs},
	get_model_data(Tail, [Head | Acc]).
	
build_db(Opts) ->
	NodeName = list_to_atom(proplists:get_value(nodename, Opts, "rpg_battlemap_dev")),
	case node() of
		nonode@nohost ->
			[] = os:cmd("epmd -daemon"),
			case net_kernel:start([NodeName, shortnames]) of
				{ok, _} ->
					ok;
				{error, {{already_started, _}, _}}  ->
					ok;
				_ ->
					erlang:error(node_fail)
			end;
		Else ->
			Else
	end,

	% get the model data
	ModelData = get_model_data(),

	mnesia:create_schema([node()]),
	io:format("Schema created\n"),
	mnesia:start(),
	% yeah hard coded stuff!
	TableProto = [{'_ids_', [key,val]} | ModelData],
	Tnames = [N || {N, _} <- TableProto],
	mnesia:wait_for_tables(Tnames, 10000),
	create_tables(TableProto).

create_tables([]) ->
	ok;

create_tables([{TableName, Attrs} | Tail]) ->
	case catch mnesia:table_info(TableName, attributes) of
		{'EXIT', {aborted, {no_exists, TableName, attributes}}} ->
			Cres = mnesia:create_table(TableName, [
				{attributes, Attrs},
				{disc_copies, [node()]}
			]),
			io:format("~s created from scratch:  ~p\n", [TableName, Cres]);
		Attrs ->
			io:format("~s already exists\n", [TableName]);
		MnesiaAttrs ->
			Ures = mnesia:transform_table(TableName, ignore, Attrs),
			io:format("~s needs to be updated:  ~p\n", [TableName, Ures]),
			io:format("    Old:  ~p\n    New:  ~p\n", [MnesiaAttrs, Attrs])
	end,
	create_tables(Tail).
%	TableData = [
%
%		% boss_db needs this to generate ids.
%		{'_ids_', [{disc_copies, [node()]}]},
%
%		% prefixing rpgb_ on table names because module 'group' is used.
%		{rpgb_user, [
%			{attributes,
%				[id, name, open_id, rpgb_group_id, created_time, updated_time]},
%			{disc_copies, [node()]}
%		]},
%
%		{rpgb_group, [
%			{attributes, [id, name, created_time, updated_time]},
%			{disc_copies, [node()]}
%		]},
%
%		{rpgb_permission, [
%			{attributes, [id, tag, rpgb_user_id, rpgb_group_id]},
%			{disc_copies, [node()]}
%		]},
%
%		{rpgb_battlemap, [
%			{attributes, [id, name, owner_id, json, created_time, updated_time]},
%			{disc_copies, [node()]}
%		]},
%
%		{rpgb_participant, [
%			{attributes, [id, battle_id, user_id]},
%			{disc_copies, [node()]}
%		]},
%
%		{rpgb_zone, [
%			{attributes, [id, name, battle_id, start_cell_x, start_cell_y,
%				layer, z_index, rotation, stroke_opactiy, stroke_color, path,
%				created_time, updated_time]},
%			{disc_copies, [node()]}
%		]}
%
%	],
%	[io:format("Creating table ~s:  ~p\n", [Tname, mnesia:create_table(Tname,
%		Attr)]) || {Tname, Attr} <- TableData].
