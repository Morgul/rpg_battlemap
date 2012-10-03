
-module(rpg_battlemap_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
        ssl:start(),
        cowboy:start(),
        supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
    

    Openid = {openid, {openid_srv, start_link, [{local, openid}]}, permanent, 
        5000, worker, [openid_srv]},

    Session = {rpgb_session, {rpgb_session, start_link, []}, permanent,
        5000, worker, [rpgb_session]},

    %Db = make_boss_db_args(Args),

    %BossNews = {boss_news, {boss_news, start, []}, permanent, 5000, worker,
    %    [boss_news]},

    %Kids = [Webmachine, Openid, Session, Db, BossNews],

%    Kids0 = case proplists:get_value(boss_cache, Args) of
%        undefined ->
%            Kids;
%        CacheArgs ->
%            CacheKid = make_cache_args(CacheArgs),
%            [CacheKid | Kids]
%    end,
    
    DataSetup = proplists:get_value(data_callback, Args),
    Data = {rpgb_data, {rpgb_data, start_link, [DataSetup]}, permanent,
        5000, worker, [rpgb_data]},

    OtherModules = proplists:get_value(additional_modules, Args, []),
    OtherModules1 = [{OmId, {OmMod, OmFunc, OmArgs}, permanent, 5000, worker, OmMods} || {OmId, OmMod, OmFunc, OmArgs, OmMods} <- OtherModules],

    Kids = [Openid, Session, Data | OtherModules1],

    {ok, { {one_for_one, 5, 10}, Kids} }.

%% -------------------------------------------------------------------

make_cache_args(Args) ->
    {boss_cache, {boss_cache, start, [Args]}, permanent, 5000, worker, [boss_cache]}.

%% -------------------------------------------------------------------

make_webmachine_args(Args) ->
    {ok, Dispatch} = file:consult(filename:join(code:priv_dir(rpg_battlemap), "dispatch.conf")),
    WebmachineDefaults = [
        {log_dir, "priv/log"},
        {ip, "0.0.0.0"},
        {port, 9090}
    ],
    Webmachine = proplists:get_value(webmachine, Args, []),
    WebmachineArgs = override_defaults(WebmachineDefaults, Webmachine),
    StartArgs = [{dispatch, Dispatch} | WebmachineArgs],
    {webmachine_mochiweb, {webmachine_mochiweb, start,
        [StartArgs]}, permanent, 5000, worker, [webmachine_mochiweb]}.

%% -------------------------------------------------------------------

make_boss_db_args(Args) ->
    BossArgs = proplists:get_value(boss_db, Args, []),
    {boss_db, {boss_db, start, [BossArgs]}, permanent, 5000, worker, [boss_db]}.

%% -------------------------------------------------------------------

override_defaults(Defaults, Args) ->
    override_defaults(Defaults, Args, []).

override_defaults([], _Args, Acc) ->
    Acc;

override_defaults([{Key, Value} = H | Tail], Args, Acc) ->
    Acc0 = case proplists:get_value(Key, Args) of
        undefined ->
            [H | Acc];
        Value ->
            [H | Acc];
        Else ->
            [{Key, Else} | Acc]
    end,
    override_defaults(Tail, Args, Acc0).
