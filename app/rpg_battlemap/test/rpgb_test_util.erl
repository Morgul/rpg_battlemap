-module(rpgb_test_util).

-include("rpg_battlemap.hrl").

-export([mecked_data/1]).
-export([get_port/1]).
-export([wait_until/1, wait_until/2, wait_until/3]).
-export([web_test_setup/1, web_test_setup/2, web_test_teardown/0,
	create_authed_session/0, create_authed_session/1,
	create_authed_session/2, assert_body/2, match_keys/2]).
-export([make_cookie/2]).
-export([start_app/1]).

start_app(AppName) ->
	case application:start(AppName) of
		ok ->
			ok;
		{error, {not_started, RequiredApp}} ->
			ok = start_app(RequiredApp),
			start_app(AppName)
	end.

make_cookie(Name, Value) when is_binary(Name) ->
	make_cookie(binary_to_list(Name), Value);
make_cookie(Name, Value) when is_binary(Value) ->
	make_cookie(Name, binary_to_list(Value));
make_cookie(Name, Value) ->
	Name ++ "=" ++ Value.

mecked_data(Callback) ->
	Ets = ets:new(Callback, [public]),
	ets:insert(Ets, {id_counter, 0}),
	meck:new(rpgb_data),
	meck:expect(rpgb_data, search, fun(Type, Params) ->
		Fields = get_fields(Type),
		Blanked = ['_' || _ <- Fields],
		BlankTuple = list_to_tuple([Type | Blanked]),
		Match = make_match(Params, Fields, BlankTuple),
		case ets:match_object(Ets, {{Type, '_'}, Match}) of
			{error, _} = Out ->
				Out;
			Objects ->
				Objs2 = [Value || {Key, Value} <- Objects],
				{ok, Objs2}
		end
	end),
	meck:expect(rpgb_data, save, fun(Rec) ->
		Type = element(1, Rec),
		{Rec1, Id} = case element(2, Rec) of
			undefined ->
				Id1 = ets:update_counter(Ets, id_counter, 1),
				{setelement(2, Rec, Id1), Id1};
			X ->
				{Rec, X}
		end,
		true = ets:insert(Ets, {{Type, Id}, Rec1}),
		{ok, Rec1}
	end),
	meck:expect(rpgb_data, delete, fun(Rec) ->
		rpgb_data:delete(element(1, Rec), element(2, Rec))
	end),
	meck:expect(rpgb_data, delete, fun(Type, Id) ->
		true = ets:delete(Ets, {Type, Id}),
		{ok, 1}
	end),
	meck:expect(rpgb_data, get_by_id, fun(Type, Id) ->
		Key = {Type, Id},
		case ets:lookup(Ets, Key) of
			[] ->
				{error, notfound};
			[{Key, Rec} | _] ->
				{ok, Rec}
		end
	end),
	meck:expect(rpgb_data, reset, fun() ->
		ets:delete_all_objects(Ets),
		ets:insert(Ets, {id_counter, 0})
	end),
	ok.

web_test_setup(TestingModule) ->
	TMList = atom_to_list(TestingModule),
	"stset_" ++ RevModule = lists:reverse(TMList),
	Module = list_to_atom(lists:reverse(RevModule)),
	web_test_setup(TestingModule, Module).

web_test_setup(TestingModule, ModuleUnderTest) ->
	application:start(ranch),
	application:start(crypto),
	application:start(cowboy),
	Port = rpgb_test_util:get_port(TestingModule),
	HostPort = {<<"localhost">>, Port},
	Routes = rpgb:get_routes(HostPort, [ModuleUnderTest]),
	Dispatch = cowboy_router:compile([{'_', Routes}]),
	cowboy:start_http(TestingModule, 1,
		[{port, Port}],
		[{env, [{dispatch, Dispatch}]}]
	),
	ibrowse:start(),
	rpgb_test_util:mecked_data(meck_data_name(TestingModule)),
	rpgb_session:make_ets().

web_test_teardown() ->
	meck:unload(rpgb_data).

create_authed_session() ->
	User = #rpgb_rec_user{
		name = <<"test_user">>
	},
	{ok, User1} = rpgb_data:save(User),
	create_authed_session(<<"test_session">>, User1).

create_authed_session(SessionId) ->
	User = #rpgb_rec_user{
		name = <<"test_user">>
	},
	{ok, User1} = rpgb_data:save(User),
	create_authed_session(SessionId, User1).

create_authed_session(SessionId, User) when is_binary(User) ->
	UserRec = #rpgb_rec_user{
		name = User
	},
	{ok, UserRec1} = rpgb_data:save(UserRec),
	create_authed_session(SessionId, UserRec1);

create_authed_session(SessionId, User) ->
	{ok, Session} = rpgb_session:get_or_create(SessionId),
	Session1 = setelement(1, Session, SessionId),
	ets:insert(rpgb_session, Session1),
	{ok, Session2} = rpgb_session:get(SessionId),
	rpgb_session:set_user(User, Session2).

meck_data_name(Module) ->
	list_to_atom(atom_to_list(Module) ++ "_data").

get_fields(rpgb_rec_user) -> record_info(fields, rpgb_rec_user);
get_fields(rpgb_rec_user_group) -> record_info(fields, rpgb_rec_user_group);
get_fields(rpgb_rec_battlemap) -> record_info(fields, rpgb_rec_battlemap);
get_fields(rpgb_rec_layer) -> record_info(fields, rpgb_rec_layer);
get_fields(rpgb_rec_zone) -> record_info(fields, rpgb_rec_zone);
get_fields(rpgb_rec_combatant) -> record_info(fields, rpgb_rec_combatant);
get_fields(rpgb_rec_character) -> record_info(fields, rpgb_rec_character).

make_match([], _Fields, Acc) ->
    Acc;
make_match([{Key, Value} | Tail], Fields, Acc) ->
    case listpos(Key, Fields) of
        {error, notfound} ->
            make_match(Tail, Fields, Acc);
        Pos ->
            Acc2 = setelement(Pos + 1, Acc, Value),
            make_match(Tail, Fields, Acc2)
    end.

listpos(Needle, Haystack) ->
    listpos(Needle, Haystack, 1).

listpos(_Needle, [], _Pos) ->
    {error, notfound};
listpos(Needle, [Needle | _Tail], Pos) ->
    Pos;
listpos(Needle, [_NotNeedle | Tail], Pos) ->
    listpos(Needle, Tail, Pos + 1).

get_port(rpgb_handle_default_tests) -> 9091;
get_port(rpgb_handle_account_tests) -> 9092;
get_port(rpgb_handle_map_tests) -> 9093;
get_port(rpgb_handle_index_tests) -> 9094;
get_port(rpgb_handle_combatant_tests) -> 9095;
get_port(rpgb_handle_character_tests) -> 9096;
get_port(rpgb_handle_layer_tests) -> 9097;
get_port(rpgb_handle_zone_tests) -> 9098;
get_port(rpgb_handle_map_websocket_tests) -> 9099.

wait_until(Fun) ->
	wait_until(Fun, 10, 100).

wait_until(Fun, Iters) ->
	wait_until(Fun, Iters, 100).

wait_until(Fun, 0, _Wait) ->
	erlang:error(wait_expired);
wait_until(Fun, Iter, Wait) ->
	case Fun() of
		true ->
			true;
		_ ->
			timer:sleep(Wait),
			wait_until(Fun, Iter - 1, Wait)
	end.

assert_body(Json, Body) when is_list(Body) ->
	assert_body(Json, list_to_binary(Body));

assert_body(Json, Body) ->
	Json1 = binary_keys(Json),
	BodyJson = jsx:to_term(Body),
	Json2 = lists:sort(Json1),
	BodyJson1 = lists:sort(BodyJson),
	match_keys(Json2, BodyJson1).

binary_keys([{}]) ->
	[];

binary_keys(L) ->
	binary_keys(L, []).

binary_keys([], Acc) ->
	lists:reverse(Acc);

binary_keys([{K, V} | Tail], Acc) when is_atom(K) ->
	binary_keys(Tail, [{list_to_binary(atom_to_list(K)), V} | Acc]);

binary_keys([{K, _} = H | Tail], Acc) when is_binary(K) ->
	binary_keys(Tail, [H | Acc]).

match_keys([], _) ->
	true;
match_keys(Remaining, []) ->
	Remaining;
match_keys([{Key, Val} | ETail], [{Key, Val} | GTail]) ->
	match_keys(ETail, GTail);
match_keys([{Key, Val} | Etail], [{Key, NotVal} | GTail]) ->
	{badmatch, Key, Val, NotVal, Etail};
match_keys(Expected, [_Got | Tail]) ->
	match_keys(Expected, Tail).
