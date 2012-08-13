-module(rpgb_dets).
-behavior(gen_server).
-behavior(rpgb_gen_data).

-include("log.hrl").
-include("rpg_battlemap.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(dets_user, rpgb_dets_user).
-define(dets_user_group, rpgb_dets_user_group).
-define(dets_battlemap, rpgb_dets_battlemap).
-define(dets_zone, rpgb_dets_zone).
-define(dets_combatant, rpgb_dets_combatant).
-define(dets_counter, rpgb_dets_counter).

% api
-export([start_link/0]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
% rpgb_gen_data
-export([
	get_user_group_by_id/1,
	get_user_group_by_name/1,
	save_user_group/1,
	delete_user_group/1,

	get_web_user_by_id/1,
	get_web_user_by_openid/1,
	get_web_user_by_name/1,
	save_web_user/1,
	delete_web_user/1,

	get_battlemap_by_id/1,
	get_battlemaps_by_owner/1,
	get_battlemaps_by_particpant/1,
	save_battlemap/1,
	delete_battlemap/1,

	get_zone_by_id/1,
	get_zones_by_battlemap/1,
	save_zone/1,
	delete_zone/1,

	get_combatant_by_id/1,
	get_combatants_by_owner/1,
	get_comtantants_by_battlemap/1,
	save_combatant/1,
	delete_combatant/1
]).

%% ====================================================================
%% External api
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_user_group_by_id(Id) ->
	Empty = empty_record_match(#user_group{}),
	Match = Empty#user_group{id = Id},
	case dets_cycle(?dets_user_group, Match, match_object) of
		[] -> notfound;
		[O | _] -> {ok, O};
		Else -> Else
	end.

get_user_group_by_name(Name) ->
	Empty = empty_record_match(#user_group{}),
	Match = Empty#user_group{name = Name},
	case dets_cycle(?dets_user_group, Match, match_object) of
		[] -> notfound;
		[O | _] -> {ok, O};
		Else -> Else
	end.

save_user_group(#user_group{id = undefined} = Rec) ->
	Id = update_counter(?dets_user_group),
	save_user_group(Rec#user_group{id = Id});

save_user_group(GroupRec) ->
	case dets_cycle(?dets_user_group, GroupRec, insert) of
		ok -> {ok, GroupRec};
		Else -> Else
	end.

delete_user_group(GroupRec) ->
	Empty = empty_record_match(#user_group{}),
	Match = Empty#user_group{id = GroupRec#user_group.id},
	dets_cycle(?dets_user_group, Match, match_delete).

get_web_user_by_id(Id) ->
	Empty = empty_record_match(#web_user{}),
	Match = Empty#web_user{id = Id},
	case dets_cycle(?dets_user, Match, match_object) of
		[] -> notfound;
		[O | _] -> {ok, O};
		Else -> Else
	end.

get_web_user_by_openid(OpenId) ->
	Empty = empty_record_match(#web_user{}),
	Match = Empty#web_user{openid = OpenId},
	case dets_cycle(?dets_user, Match, match_object) of
		[] -> notfound;
		[O | _] -> {ok, O};
		Else -> Else
	end.

get_web_user_by_name(Name) ->
	Empty = empty_record_match(#web_user{}),
	Match = Empty#web_user{name = Name},
	case dets_cycle(?dets_user, Match, match_object) of
		[] -> notfound;
		[O | _] -> {ok, O};
		Else -> Else
	end.

save_web_user(#web_user{id = undefined} = UserRec) ->
	Id = update_counter(?dets_user),
	save_web_user(UserRec#web_user{id = Id});

save_web_user(UserRec) ->
	case dets_cycle(?dets_user, UserRec, insert) of
		ok -> {ok, UserRec};
		Else -> Else
	end.

delete_web_user(UserRec) ->
	Id = UserRec#web_user.id,
	Empty = empty_record_match(UserRec),
	Match = Empty#web_user{id = Id},
	dets_cycle(?dets_user, Match, match_delete).

get_battlemap_by_id(MapId) ->
	Empty = empty_record_match(#battlemap{}),
	Match = Empty#battlemap{id = MapId},
	case dets_cycle(?dets_battlemap, Match, match_object) of
		[] -> notfound;
		[O | _] -> {ok, O};
		Else -> Else
	end.

get_battlemaps_by_owner(Id) ->
	Empty = empty_record_match(#battlemap{}),
	Match = Empty#battlemap{owner_id = Id},
	case dets_cycle(?dets_battlemap, Match, match_object) of
		L when is_list(L) -> {ok, L};
		Else -> Else
	end.

get_battlemaps_by_particpant(UserRec) ->
	{ok, Table} = dets:open_file(?dets_battlemap),
	Userid = UserRec#web_user.id,
	Qh = qlc:q([M || #battlemap{participant_ids = P} = M <- dets:table(Table),
		lists:member(Userid, P)]),
	case qlc:e(Qh) of
		L when is_list(L) -> {ok, L};
		Else -> Else
	end.

save_battlemap(#battlemap{id = undefined} = MapRec) ->
	Id = update_counter(?dets_battlemap),
	save_battlemap(MapRec#battlemap{id = Id});

save_battlemap(MapRec) ->
	case dets_cycle(?dets_battlemap, MapRec, insert) of
		ok -> {ok, MapRec};
		Else -> Else
	end.

delete_battlemap(#battlemap{id = Id}) ->
	E = empty_record_match(#battlemap{}),
	M = E#battlemap{id = Id},
	dets_cycle(?dets_battlemap, M, match_delete).

get_zone_by_id(Id) ->
	Empty = empty_record_match(#zone{}),
	Match = Empty#zone{id = Id},
	case dets_cycle(?dets_zone, Match, match_object) of
		[] -> notfound;
		[O|_] -> {ok, O};
		E -> E
	end.

get_zones_by_battlemap(MapId) ->
	E = empty_record_match(#zone{}),
	M = E#zone{battlemap_id = MapId},
	case dets_cycle(?dets_zone, M, match_object) of
		L when is_list(L) -> {ok, L};
		El -> El
	end.

save_zone(#zone{id = undefined} = Z) ->
	Id = update_counter(?dets_zone),
	save_zone(Z#zone{id = Id});

save_zone(ZoneRec) ->
	case dets_cycle(?dets_zone, ZoneRec, insert) of
		ok -> {ok, ZoneRec};
		E -> E
	end.

delete_zone(#zone{id = Id}) ->
	E = empty_record_match(#zone{}),
	M = E#zone{id = Id},
	dets_cycle(?dets_zone, M, match_delete).

get_combatant_by_id(Id) ->
	E = empty_record_match(#combatant{}),
	M = E#combatant{id = Id},
	case dets_cycle(?dets_combatant, M, match_object) of
		[] -> notfound;
		[O|_] -> {ok, O};
		El -> El
	end.

get_combatants_by_owner(OwnerId) ->
	E = empty_record_match(#combatant{}),
	M = E#combatant{owner_id = OwnerId},
	case dets_cycle(?dets_combatant, M, match_object) of
		L when is_list(L) -> {ok, L};
		El -> El
	end.

get_comtantants_by_battlemap(MapId) ->
	E = empty_record_match(#combatant{}),
	M = E#combatant{battlemap_id = MapId},
	case dets_cycle(?dets_combatant, M, match_object) of
		L when is_list(L) -> {ok, L};
		El -> El
	end.

save_combatant(#combatant{id = undefined} = C) ->
	Id = update_counter(?dets_combatant),
	save_combatant(C#combatant{id = Id});

save_combatant(CombatantRec) ->
	case dets_cycle(?dets_combatant, CombatantRec, insert) of
		ok -> {ok, CombatantRec};
		E -> E
	end.

delete_combatant(Id) ->
	E = empty_record_match(#combatant{}),
	M = E#combatant{id = Id},
	dets_cycle(?dets_combatant, M, match_delete).

%% ====================================================================
%% gen_server
%% ====================================================================

%% --------------------------------------------------------------------
%% Init
%% --------------------------------------------------------------------

init(_) ->
	Counters = [?dets_user, ?dets_user_group, ?dets_battlemap, ?dets_zone, 
		?dets_combatant],
	Priv = code:priv_dir(rpg_battlemap),
	{ok, CounterDets} = dets:open_file(?dets_counter, [{file, filename:join(Priv, ?dets_counter)}]),
	CounterObjs = [{C, 0} || C <- Counters],
	dets:insert_new(CounterDets, CounterObjs),
	[dets:open_file(C, [{keypos, 2}, {file, filename:join(Priv, C)}]) || C <- Counters],
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
	dets:close(?MODULE),
	dets:close(rpgb_dets_counter).

%% --------------------------------------------------------------------
%% code_change
%% --------------------------------------------------------------------

code_change(_Meh, State, _Xtra) ->
	{ok, State}.

%% ====================================================================
%% internal
%% ====================================================================

empty_record_match(Tuple) when size(Tuple) > 0 ->
	[Recname | Fields] = tuple_to_list(Tuple),
	Fields0 = ['_' || _ <- Fields],
	list_to_tuple([Recname | Fields0]).

dets_cycle(Name, Match, Func) when Func == match_delete; Func == match_object; Func == insert ->
	F = filename:join(code:priv_dir(rpg_battlemap), Name),
	{ok, T} = dets:open_file(Name, [{keypos, 2}, {file, F}]),
	Res = dets:Func(T, Match),
	dets:close(T),
	Res.

update_counter(Counter) ->
	F = filename:join(code:priv_dir(rpg_battlemap), ?dets_counter),
	{ok, T} = dets:open_file(?dets_counter, [{file, F}]),
	Res = dets:update_counter(T, Counter, 1),
	dets:close(T),
	Res.
