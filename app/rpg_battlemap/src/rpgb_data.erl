-module(rpgb_data).

-behavior(gen_server).

-include("log.hrl").
-include("rpg_battlemap.hrl").

% api
-export([
	start_link/1,

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
	get_battlemaps_by_participant/1,
	save_battlemap/1,
	delete_battlemap/1,

	get_zone_by_id/1,
	get_zones_by_battlemap/1,
	save_zone/1,
	delete_zone/1,

	get_combatant_by_id/1,
	get_combatants_by_owner/1,
	get_combatants_by_battlemap/1,
	save_combatant/1,
	delete_combatant/1
]).

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% ====================================================================
%% External api
%% ====================================================================

start_link(Callback) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Callback, []).

get_user_group_by_id(Id) ->
	callback(get_user_group_by_id, Id).

get_user_group_by_name(Name) ->
	callback(get_user_group_by_name, Name).

save_user_group(GroupRec) ->
	Now = os:timestamp(),
	GroupRec1 = case GroupRec#user_group.created of
		undefined ->
			GroupRec#user_group{ created = Now, updated = Now};
		_ ->
			GroupRec#user_group{updated = Now}
	end,
	callback(save_user_group, GroupRec1).

delete_user_group(GroupRec) ->
	callback(delete_user_group, GroupRec).

get_web_user_by_id(Id) ->
	callback(get_web_user_by_id, Id).

get_web_user_by_openid(OpenId) ->
	callback(get_web_user_by_openid, OpenId).

get_web_user_by_name(Name) ->
	callback(get_web_user_by_name, Name).

save_web_user(UserRec) ->
	Now = os:timestamp(),
	UserRec1 = case UserRec#web_user.created of
		undefined ->
			UserRec#web_user{created = Now, updated = Now};
		_ ->
			UserRec#web_user{updated = Now}
	end,
	callback(save_web_user, UserRec1).

delete_web_user(UserRec) ->
	callback(delete_web_user, UserRec).

get_battlemap_by_id(MapId) ->
	callback(get_battlemap_by_id, MapId).

get_battlemaps_by_owner(#web_user{id = Id}) ->
	get_battlemaps_by_owner(Id);

get_battlemaps_by_owner(OwnerId) ->
	callback(get_battlemaps_by_owner, OwnerId).

get_battlemaps_by_participant(#web_user{id = Id}) ->
	get_battlemaps_by_participant(Id);

get_battlemaps_by_participant(UserId) ->
	callback(get_battlemaps_by_particpant, UserId).

save_battlemap(MapRec) ->
	Now = os:timestamp(),
	Map1 = case MapRec#battlemap.created of
		undefined ->
			MapRec#battlemap{created = Now, updated = Now};
		_ ->
			MapRec#battlemap{updated = Now}
	end,
	callback(save_battlemap, Map1).

delete_battlemap(MapRec) ->
	callback(delete_battlemap, MapRec).

get_zone_by_id(Id) ->
	callback(get_zone_by_id, Id).

get_zones_by_battlemap(#battlemap{id = Id}) ->
	get_zones_by_battlemap(Id);

get_zones_by_battlemap(MapId) ->
	callback(get_zones_by_battlemap, MapId).

save_zone(ZoneRec) ->
	Now = os:timestamp(),
	ZoneRec1 = case ZoneRec#zone.created of
		undefined ->
			ZoneRec#zone{created = Now, updated = Now};
		_ ->
			ZoneRec#zone{updated = Now}
	end,
	callback(save_zone, ZoneRec1).

delete_zone(ZoneRec) ->
	callback(delete_zone, ZoneRec).

get_combatant_by_id(Id) ->
	callback(get_combatant_by_id, Id).

get_combatants_by_owner(#web_user{id = Id}) ->
	get_combatants_by_owner(Id);

get_combatants_by_owner(OwnerId) ->
	callback(get_combatants_by_owner, OwnerId).

get_combatants_by_battlemap(#battlemap{id = Id}) ->
	get_combatants_by_battlemap(Id);

get_combatants_by_battlemap(MapId) ->
	callback(get_combatants_by_battlemap, MapId).

save_combatant(CombatantRec) ->
	Now = os:timestamp(),
	Combatant = case CombatantRec#combatant.created of
		undefined ->
			CombatantRec#combatant{created = Now, updated = Now};
		_ ->
			CombatantRec#combatant{updated = Now}
	end,
	callback(save_combatant, Combatant).

delete_combatant(Combatant) ->
	callback(delete_combatant, Combatant).

%% ====================================================================
%% Gen server callbacks
%% ====================================================================

%% --------------------------------------------------------------------
%% Init
%% --------------------------------------------------------------------

init(Callback) ->
	{ok, Callback}.

%% --------------------------------------------------------------------
%% handle_call
%% --------------------------------------------------------------------

handle_call({callback, Function, Args}, _From, Callback) ->
	Res = apply(Callback, Function, Args),
	{reply, Res, Callback};

handle_call(_Msg, _From, Callback) ->
	{reply, {error, invalid}, Callback}.

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
%% terminate
%% --------------------------------------------------------------------

terminate(_Why, _State) -> ok.

%% --------------------------------------------------------------------
%% code_change
%% --------------------------------------------------------------------

code_change(_Huh, State, _Xtra) ->
	{ok, State}.

%% ====================================================================
%% Internal Functions
%% ====================================================================

callback(Function, Args) when is_list(Args) ->
	gen_server:call(?MODULE, {callback, Function, Args});
callback(Function, Args) ->
	callback(Function, [Args]).
