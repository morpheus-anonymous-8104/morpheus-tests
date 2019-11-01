-module(ra_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

all_test_() ->
    {timeout, 3600, ?_test( test_entry() )}.

-define(config(Name, Config), proplists:get_value(Name, Config)).

-define(T, morpheus_test_lib).
-define(S, morpheus_sandbox).
-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).

test_entry() ->
    InitConfig =
        [ {server_id,  {tserver1, node()}}, {uid, <<"node1_uid">>}
        , {server_id2, {tserver2, node()}}, {uid2, <<"node2_uid">>}
        , {server_id3, {tserver3, node()}}, {uid3, <<"node3_uid">>}
        , {server_id4, {tserver4, node()}}, {uid4, <<"node4_uid">>}
        , {server_id5, {tserver5, node()}}, {uid5, <<"node5_uid">>}
        , {cluster_name, <<"cluster">>}
        , {priv_dir, "ra_data"}
        , {testcase, list_to_atom(os:getenv("TESTCASE"))}
        ],
    os:cmd("rm -rf ra_data/*"),
    ?T:start_morpheus_test(ra_test, test_inner_entry, InitConfig),
    ok.

test_inner_entry(Config) ->
    ?GH:bootstrap(),
    ok = application:load(ra),
    application:ensure_all_started(lg),
    Testcase = proplists:get_value(testcase, Config),
    ra:start_in(proplists:get_value(priv_dir, Config)),
    apply(ra_test, Testcase, [Config]),
    application:stop(ra),
    timer:sleep(1000),
    ?G:exit_with(success),
    ok.

%% ra-1
badkey_previous_cluster(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    %% test starts ========================================
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),
    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],
    ?GH:sync_task([ par
                  , fun () -> catch ra:trigger_election(ServerId1) end
                  , fun () -> catch ra:trigger_election(ServerId2) end
                  , fun () -> catch ra:trigger_election(ServerId3) end
                  , fun () -> catch ra:trigger_election(ServerId4) end
                  , fun () -> catch ra:remove_member(ServerId1, ServerId2),
                              catch ra:add_member(ServerId1, ServerId2) end
                  , fun () -> catch ra:remove_member(ServerId1, ServerId3),
                              catch ra:add_member(ServerId1, ServerId3) end
                  , fun () -> catch ra:remove_member(ServerId5, ServerId4),
                              catch ra:add_member(ServerId5, ServerId4) end
                  , fun () -> R = (catch enqueue(ServerId3, msg1)),
                              ets:insert(test_tab, {msg1, R}) end
                  ]),
    timer:sleep(10000),
    %% test ends ========================================
    case ets:lookup(test_tab, msg1) of
        [{_, ok}] ->
            Out = (catch dequeue(ServerId1)),
            case Out of
                msg1 -> ok;
                _ -> error(dequeue_doesnt_match)
            end;
        [{_, R}] -> ok
    end,
    ets:delete(test_tab),
    ok.

%% ra-2
inconsistent_state(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    Msgs = [msg1, msg2, msg3, msg4, msg5],
    %% test start ========================================
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),
    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],
    ?GH:sync_task([ par
                  , fun () -> catch ra:trigger_election(ServerId1) end
                  , fun () -> catch ra:trigger_election(ServerId2) end
                  , fun () -> catch ra:trigger_election(ServerId3) end
                  , fun () -> catch ra:remove_member(ServerId4, ServerId5),
                              catch ra:add_member(ServerId4, ServerId5) end
                  , fun () ->
                            lists:foreach(
                              fun (D) ->
                                      S = lists:nth(rand:uniform(length(Peers)), Peers),
                                      R = (catch enqueue(S, D)),
                                      ets:insert(test_tab, {D, R})
                              end, Msgs)
                    end
                  ]),
    timer:sleep(10000),
    %% test end ========================================
    Expected = (catch ra:local_query(ServerId1, fun (S) -> S end)),
    States = lists:foreach(
               fun (ServerId) ->
                       S = (catch ra:local_query(ServerId, fun (S) -> S end)),
                       case S =:= Expected of
                           true -> ok;
                           false -> case {Expected, S} of
                                        {{'EXIT', _}, _} -> ok;
                                        {_, {'EXIT', _}} -> ok;
                                        {_, _} -> error(unexpected)
                                    end
                       end
               end, Peers -- [ServerId1]),
    ets:delete(test_tab),
    ok.

%% ra-3
inconsistent_state_2(Config) ->
    ClusterName = ?config(cluster_name, Config),
    PrivDir = ?config(priv_dir, Config),
    ServerId1 = ?config(server_id, Config),
    ServerId2 = ?config(server_id2, Config),
    ServerId3 = ?config(server_id3, Config),
    ServerId4 = ?config(server_id4, Config),
    ServerId5 = ?config(server_id5, Config),
    Peers = [ServerId1, ServerId2, ServerId3, ServerId4, ServerId5],
    Msgs = [msg1, msg2, msg3, msg4, msg5],
    %% test start ========================================
    ok = start_cluster(ClusterName, Peers),
    test_tab = ets:new(test_tab, [public, named_table]),
    timer:sleep(10000),
    [ begin
          UId = ra_directory:uid_of(Name),
          ?assert(filelib:is_dir(filename:join([ra_env:data_dir(), UId])))
      end || {Name, _} <- Peers],
    ?GH:sync_task([ par
                  , fun () -> R = (catch enqueue(ServerId3, msg1)),
                              R1 = (catch ra:remove_member(ServerId2, ServerId3)),
                              R2 = (catch ra:add_member(ServerId2, ServerId3)),
                              ets:insert(test_tab, {msg1, R}),
                              ok end
                  , fun () -> R = (catch enqueue(ServerId3, msg2)),
                              R1 = (catch ra:remove_member(ServerId4, ServerId3)),
                              R2 = (catch ra:add_member(ServerId4, ServerId3)),
                              ets:insert(test_tab, {msg2, R}),
                              ok end
                  , fun () -> catch ra:trigger_election(ServerId1) end
                  , fun () -> catch ra:trigger_election(ServerId5) end
                  ]),
    timer:sleep(10000),
    %% test end ========================================
    Expected = (catch ra:local_query(ServerId1, fun (S) -> S end)),
    States = lists:foreach(
               fun (ServerId) ->
                       S = (catch ra:local_query(ServerId, fun (S) -> S end)),
                       case S =:= Expected of
                           true -> ok;
                           false ->
                               case {Expected, S} of
                                   {{'EXIT', _}, _} -> ok;
                                   {_, {'EXIT', _}} -> ok;
                                   {_, _} -> error(unexpected)
                               end
                       end
               end, Peers -- [ServerId1]),
    ExpectedResult = lists:foldl(
                       fun (M, Acc) -> 
                               case ets:lookup(test_tab, M) of
                                   [{_, ok}] -> [M | Acc];
                                   _ -> Acc
                               end
                       end, [], [msg1, msg2]),
    DequeueResult =
        lists:foldl(fun (_, Acc) -> [(catch dequeue(ServerId3)) | Acc] end, [], [msg1, msg2]),
    case [] =:= ExpectedResult -- DequeueResult of
        true -> ok;
        false -> error(unexpected)
    end,
    ets:delete(test_tab),
    ok.

%% Helpers copied from ra_2_SUITE.erl ==================================================

start_cluster(ClusterName, ServerIds, Config) ->
    {ok, Started, _} = ra:start_cluster(ClusterName,
                                        {module, ?MODULE, Config},
                                        ServerIds),
    ?assertEqual(lists:sort(ServerIds), lists:sort(Started)),
    ok.

start_cluster(ClusterName, ServerIds) ->
    start_cluster(ClusterName, ServerIds, #{}).

enqueue(Server, Msg) ->
    {ok, _, _} = ra:process_command(Server, {enq, Msg}),
    ok.

dequeue(Server) ->
    {ok, Res, _} = ra:process_command(Server, deq),
    Res.

%% ra_machine test impl
init(_) ->
    queue:new().

'apply'(_Meta, {enq, Msg}, Effects, State) ->
    {queue:in(Msg, State), Effects, ok};
'apply'(_Meta, deq, Effects, State0) ->
    case queue:out(State0) of
        {{value, Item}, State} ->
            {State, Effects, Item};
        {empty, _} ->
            {State0, Effects, empty}
    end;
'apply'(_Meta, {deq, Pid}, Effects, State0) ->
    case queue:out(State0) of
        {{value, Item}, State} ->
            {State, [{send_msg, Pid, Item, ra_event} | Effects], ok};
        {empty, _} ->
            {State0, Effects, ok}
    end.

state_enter(eol, State) ->
    [{send_msg, P, eol, ra_event} || {P, _} <- queue:to_list(State), is_pid(P)];
state_enter(_, _) ->
    [].
