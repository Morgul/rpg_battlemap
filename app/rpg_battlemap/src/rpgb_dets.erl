-module(rpgb_dets).
-behavior(gen_server).
-behavior(rpgb_gen_data).

-include("log.hrl").
-include("rpg_battlemap.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(dets_table, rpgb_dets).

% api
-export([start_link/0, start_link/1]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
% rpgb_gen_data
-export([
  get_by_id/2,
  save/1,
  delete/2,
  search/2
]).

%% ====================================================================
%% External api
%% ====================================================================

start_link() ->
  start_link([]).

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

get_by_id(Type, Id) ->
  case dets:lookup(?dets_table, {Type, Id}) of
    [] -> {error, notfound};
    [O | _] -> {ok, O}
  end.

save(Rec) ->
  Rec1 = case element(2, Rec) of
    undefined ->
      NewId = dets:update_counter(?dets_table, element(1, Rec), 1),
      setelement(2, Rec, NewId);
    _ ->
      Rec
  end,
  Type = element(1, Rec1),
  Id = element(2, Rec1),
  case dets:insert(?dets_table, {{Type, Id}, Rec1}) of
    ok ->
      {ok, Rec1};
    Err ->
      Err
  end.

delete(Type, Id) ->
  case dets:delete(?dets_table, {Type, Id}) of
    ok ->
      {ok, 1};
    Err ->
      Err
  end.

search(Type, Params) ->
  FieldNames = get_field_names(Type),
  BlankTuple1 = ['_' || _ <- FieldNames],
  BlankTuple2 = [Type | BlankTuple1],
  BlankRec = list_to_tuple(BlankTuple2),
  FieldIndexs = indexize(FieldNames, 1),
  Match = build_match(Params, FieldIndexs, BlankRec),
  case dets:match_object(?dets_table, {{Type, '_'}, Match}) of
    Objs when is_list(Objs) ->
      {ok, Objs};
    E ->
      E
  end.

%% ====================================================================
%% gen_server
%% ====================================================================

%% --------------------------------------------------------------------
%% Init
%% --------------------------------------------------------------------

init(Options) ->
  DataDir = case proplists:get_value(data_dir, Options) of
    undefined ->
      Dir1 = filename:join(code:priv_dir(rpg_battlemap), "data"),
      ok = filelib:ensure_dir(Dir1),
      Dir1;
    Dir ->
      Dir
  end,
  {ok, _} = dets:open_file(?dets_table, [{file, DataDir}]),
  Counters = [ rpgb_rec_user, rpgb_rec_user_group, rpgb_rec_battlemap,
    rpgb_rec_zone, rpgb_rec_combatant, rpgb_rec_character,
    rpgb_rec_layer ],
  [dets:insert_new({C, 0}) || C <- Counters],
  {ok, undefined}.

%% --------------------------------------------------------------------
%% handle_call
%% --------------------------------------------------------------------

handle_call(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

%% --------------------------------------------------------------------
%% handle_cast
%% --------------------------------------------------------------------

handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% handle_info
%% --------------------------------------------------------------------

handle_info(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% termiante
%% --------------------------------------------------------------------

terminate(_Meh, _State) ->
	dets:close(?dets_table).

%% --------------------------------------------------------------------
%% code_change
%% --------------------------------------------------------------------

code_change(_Meh, State, _Xtra) ->
	{ok, State}.

%% ====================================================================
%% internal
%% ====================================================================

get_field_names(rpgb_rec_user) -> record_info(fields, rpgb_rec_user);
get_field_names(rpgb_rec_user_group) -> record_info(fields, rpgb_rec_user_group);
get_field_names(rpgb_rec_battlemap) -> record_info(fields, rpgb_rec_battlemap);
get_field_names(rpgb_rec_layer) -> record_info(fields, rpgb_rec_layer);
get_field_names(rpgb_rec_zone) -> record_info(fields, rpgb_rec_zone);
get_field_names(rpgb_rec_combatant) -> record_info(fields, rpgb_rec_combatant);
get_field_names(rpgb_rec_character) -> record_info(fields, rpgb_rec_character).

indexize(Items, StartIndex) ->
  indexize(Items, StartIndex, []).

indexize([], _Index, Acc) ->
  lists:reverse(Acc);

indexize([Head | Tail], Index, Acc) ->
  indexize(Tail, Index + 1, [{Head, Index} | Acc]).

build_match([], _Indexs, Rec) ->
  Rec;
build_match([{Key, Value} | Tail], Indexes, Rec) ->
  case proplists:get_value(Key, Indexes) of
    undefined ->
      build_match(Tail, Indexes, Rec);
    N ->
      Rec1 = setelement(N, Rec, Value),
      build_match(Tail, Indexes, Rec1)
  end.
