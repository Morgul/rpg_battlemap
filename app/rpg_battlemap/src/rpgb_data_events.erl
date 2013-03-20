-module(rpgb_data_events).

% public api
-export([start_link/0, stop/0, subscribe/2, notify/1]).

% public api
start_link() ->
	gen_event:start_link({local, ?MODULE}).

stop() ->
	gen_event:stop(?MODULE).

subscribe(Callback, Args) ->
	Self = self(),
	gen_event:add_sup_handler(?MODULE, Callback, Args).

notify(Msg) ->
	gen_event:notify(?MODULE, Msg).
