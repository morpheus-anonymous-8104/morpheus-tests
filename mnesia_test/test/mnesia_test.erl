-module(mnesia_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

all_test_() ->
    {timeout, 86400, ?_test( test_entry() )}.

-define(config(Key, Data), proplists:get_value(Key, Data)).

-define(T, morpheus_test_lib).
-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

test_entry() ->
    ?T:start_morpheus_test(mnesia_test, list_to_atom(os:getenv("TESTCASE")), [{nodes, [node1@localhost, node2@localhost]}, {master_node, node1@localhost}]).

t_sandbox_entry(Config) ->
    FNAME = ?config(testcase, Config),
    apply(?MODULE, FNAME, [Config]),
    ?G:exit_with(success).

prepare(Config) ->
    os:cmd("rm -r data MnesiaCore.*"),
    os:cmd("mkdir data"),
    Nodes = ?config(nodes, Config),
    lists:foreach(
      fun (N) -> ?GH:bootstrap_remote(N),
                 ok = rpc:call(N, application, set_env, [mnesia, dir, lists:flatten(io_lib:format("data/~w", [N]))])
      end, Nodes),
    ok = mnesia:create_schema(Nodes),
    lists:foreach(fun (N) -> {ok, _} = rpc:call(N, application, ensure_all_started, [mnesia]) end,
                  ?config(nodes, Config)),
    ok.

%% mnesia-1
add_copy_and_restart(Config) ->
    ok = prepare(Config),
    {atomic, ok} = mnesia:create_table(test_tab, [{disc_copies, [node1@localhost]}]),
    Self = self(),
    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    (fun Retry() ->
             case mnesia:add_table_copy(test_tab, node2@localhost, disc_copies) of
                 {atomic, ok} -> io:format(user, "add_table_copy ok~n", []), ok;
                 {aborted, {already_exists, _, _}} ->
                     io:format(user, "add_table_copy already exists?~n", []), ok;
                 _R ->
                     io:format(user, "add_table_copy ~p~n", [_R]),
                     timer:sleep(100),
                     Retry()
             end
     end)(),
    receive ok -> ok end,
    timer:sleep(100),
    case lists:member(node2@localhost, mnesia:table_info(test_tab, disc_copies)) of
        true -> ?G:exit_with(success);
        _R -> io:format(user, "result mismatch: ~w~n", [_R]),
              ?G:exit_with(result_mismatch)
    end.

%% mnesia-2
del_copy_and_restart(Config) ->
    ok = prepare(Config),
    {atomic, ok} = mnesia:create_table(test_tab, [{disc_copies, [node1@localhost, node2@localhost]}]),
    Self = self(),
    spawn(node2@localhost,
          fun () ->
                  application:stop(mnesia),
                  ok = application:start(mnesia),
                  Self ! ok
          end),
    (fun Retry() ->
             case mnesia:del_table_copy(test_tab, node2@localhost) of
                 {atomic, ok} -> io:format(user, "del_table_copy ok~n", []), ok;
                 _R -> io:format(user, "del_table_copy ~p~n", [_R]),
                       timer:sleep(100),
                       Retry()
             end
     end)(),
    receive ok -> ok end,
    timer:sleep(100),
    case mnesia:table_info(test_tab, disc_copies) of
        [node1@localhost] -> ?G:exit_with(success);
        _R -> io:format(user, "result mismatch: ~w~n", [rpc:multicall(?config(nodes, Config), mnesia, table_info, [test_tab, disc_copies])]),
              ?G:exit_with(result_mismatch)
    end.
