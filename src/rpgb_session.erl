-module(rpgb_session).

-behavior(gen_server).

-include("log.hrl").
-include_lib("stdlib/include/qlc.hrl").

% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
% api
-export([start_link/0, start_link/1, get_or_create/1, get/1, create/0,
	destroy/1, get_id/1, get_user/1, get_value/2, get_value/3, set_user/2]).

%% =================================================================
%% Api
%% =================================================================

start_link() -> start_link([]).

start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

get_or_create(Id) when is_list(Id) ->
	get_or_create(list_to_binary(Id));

get_or_create(Id) when is_binary(Id) ->
	case ?MODULE:get(Id) of
		{error, notfound} -> create();
		E -> E
	end;

get_or_create(ReqData) ->
	?info("req data version of get_or_create", []),
	{Newness, {ok, Session}} = case ?MODULE:get(ReqData) of
		{error, notfound} -> {new, create()};
		E -> {old, E}
	end,
	case Newness of
		old ->
			?info("session already exists:  ~p", [get_id(Session)]),
			{ok, Session, ReqData};
		new ->
			SessionId1 = get_id(Session),
			?info("session was created:  ~p", [SessionId1]),
			{CookieHKey, CookieHVal} = mochiweb_cookies:cookie("rpgbsid",
				SessionId1, [{max_age, 60 * 60 * 24 * 7}]),
			ReqData0 = wrq:set_resp_header(CookieHKey, CookieHVal, ReqData),
			{ok, Session, ReqData0}
	end.

get(undefined) ->
	{error, notfound};

get(Id) when is_list(Id) ->
	?MODULE:get(list_to_binary(Id));

get(Id) when is_binary(Id) ->
	QH = qlc:q([X || {SessId, _, _, _} = X <- ets:table(?MODULE),
		SessId =:= Id]),
	case qlc:e(QH) of
		[] -> {error, notfound};
		[Session] ->
			ets:update_element(?MODULE, Id, {4, calendar:local_time()}),
			{ok, Session}
	end;

get(ReqData) ->
	?info("getting session based on req data"),
	?debug("Das reqData:  ~p", [ReqData]),
	SessionId = wrq:get_cookie_value("rpgbsid", ReqData),
	?MODULE:get(SessionId).

create() ->
	Uuid = make_uuid(),
	Session = {Uuid, undefined, dict:new(), calendar:local_time()},
	case ets:insert_new(?MODULE, Session) of
		false ->
			create();
		true ->
			{ok, Session}
	end.

destroy(Id) ->
	ets:delete(?MODULE, Id).

get_id({Id, _, _, _}) -> Id.

get_user({_, User, _, _}) -> User.

set_user(User, {Id, undefined, Values, _}) ->
	Session = {Id, User, Values, calendar:local_time()},
	ets:insert(?MODULE, Session),
	{ok, Session};

set_user(_, _) ->
	{error, user_already_set}.

get_value(Key, {_, _, Dict, _}) ->
	dict:find(Key, Dict).

get_value(Key, {_, _, Dict, _}, Default) ->
	case dict:find(Key, Dict) of
		error -> {ok, Default};
		E -> E
	end.

%% =================================================================
%% Init
%% =================================================================

init(_Opts) ->
	Ets = ets:new(?MODULE, [named_table, public]),
	Self = self(),
	Timer = erlang:send_after(1000 * 60 * 60, Self, clear_dead_sessions),
	{ok, {Timer, Ets}}.

%% =================================================================
%% handle_call
%% =================================================================

handle_call(_Msg, _From, State) ->
	{reply, {error, unhandled}, State}.

%% =================================================================
%% handle_cast
%% =================================================================

handle_cast(_Msg, State) ->
	{noreply, State}.

%% =================================================================
%% handle_info
%% =================================================================

handle_info(clear_dead_session, {_, Ets}) ->
	Now = calendar:local_time(),
	NowSecs = calendar:datetime_to_gregorian_seconds(Now),
	QH = qlc:q([Id || {Id, _, _, LastTouch} <- ets:table(Ets),
		too_old(LastTouch, NowSecs)]),
	Ids = qlc:e(QH),
	[ets:delete(Ets, I) || I <- Ids],
	Self = self(),
	Timer = erlang:send_after(1000 * 60 * 60, Self, clear_dead_sessions),
	{noreply, {Timer, Ets}};
	
handle_info(_Msg, State) ->
	{noreply, State}.

too_old(TestDate, Now) ->
	TestSecs = calendar:datetime_to_gregorian_seconds(TestDate),
	ThreeHours = 1000 * 60 * 60 * 24 * 8,
	TimeDiff = Now - TestSecs,
	if
		TimeDiff > ThreeHours -> true;
		true -> false
	end.
	
%% =================================================================
%% terminate
%% =================================================================

terminate(_Why, _State) -> ok.

%% =================================================================
%% code_change
%% =================================================================

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% =================================================================
%% Internal Functions
%% =================================================================

make_uuid() ->
	make_uuid(crypto:rand_bytes(16)).

make_uuid(Bin) when is_binary(Bin) ->
	Hexstr = rpgb_util:bin_to_hexstr(Bin),
	Hexbin = list_to_binary(Hexstr),
	<<Eight:8/binary, Four1:4/binary, _:1/binary, Four2:3/binary, _:1/binary, Four3:3/binary, End/binary>> = Hexbin,
	<<Eight/binary, $-, Four1/binary, $-, $4, Four2/binary, $-, $a, Four3/binary, $-, End/binary>>.
	
%% =================================================================
%% Tests
%% =================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

make_uuid_test() ->
	Bin = <<33,190,233,81,19,2,56,20,150,182,80,133,173,234,131,6>>,
	Expected = <<"21bee951-1302-4814-a6b6-5085adea8306">>,
	?assertEqual(Expected, make_uuid(Bin)).

-endif.
