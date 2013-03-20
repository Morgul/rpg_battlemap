-module(rpgb_data).

-behavior(gen_server).

-include("log.hrl").
-include("rpg_battlemap.hrl").

% api
-export([
	start_link/1,

	get_by_id/2,
	search/2,
	save/1,
	delete/1,
	delete/2,

	transaction/1

]).


% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% ====================================================================
%% External api
%% ====================================================================

start_link(Callback) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Callback, []).

get_by_id(Type, Id) ->
	gen_server:call(?MODULE, {api, get_by_id, [Type, Id]}).

search(Type, SearchParams) ->
	gen_server:call(?MODULE, {api, search, [Type, SearchParams]}).

save(Record) ->
	gen_server:call(?MODULE, {api, save, [Record]}).

delete(Record) ->
	delete(element(1, Record), element(2, Record)).

delete(Type, Id) ->
	gen_server:call(?MODULE, {api, delete, [Type, Id]}).

transaction(Fun) ->
	gen_server:call(?MODULE, {api, transaction, [Fun]}).

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

handle_call({api, Function, Args}, From, Callback) ->
	proc_lib:spawn(fun() ->
		Res = apply(Callback, Function, Args),
		gen_server:reply(From, Res),
		maybe_broadcast(Function, Args, Res)
	end),
	{noreply, Callback};

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

maybe_broadcast(save, [OldRecord], {ok, NewRecord}) ->
	Msg = case element(2, OldRecord) of
		undefined ->
			{new, NewRecord};
		_ ->
			{update, NewRecord}
	end,
	broadcast(Msg);

maybe_broadcast(delete, [Type, Id], {ok, _N}) ->
	broadcast({delete, Type, Id});

maybe_broadcast(_Function, _Args, _Result) ->
	ok.

broadcast(Msg) ->
	rpgb_data_events:notify(Msg).