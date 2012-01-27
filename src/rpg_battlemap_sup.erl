
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
	Dispatch = case proplists:get_value(dispatch, Args, "priv/dispatch.conf") of
		X when is_list(X) -> {ok, Out} = file:consult(X), Out;
		X -> X
	end,
	Defaults = [
		{log_dir, "priv/log"},
		{ip, "0.0.0.0"},
		{port, 9090}
	],
	Args0 = proplists:delete(dispatch, Args),
	StartArgs = [{dispatch, Dispatch} | override_defaults(Defaults, Args0)],
	Kid = {webmachine_mochiweb, {webmachine_mochiweb, start, [StartArgs]},
		permanent, 5000, worker, [webmachine_mochiweb]},
    {ok, { {one_for_one, 5, 10}, [Kid]} }.

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
