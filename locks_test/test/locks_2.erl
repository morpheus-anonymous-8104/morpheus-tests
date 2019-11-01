-module(locks_2).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

-define(T, morpheus_test_lib).
-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

test_entry() ->
    ?T:start_morpheus_test(locks_2, locks_2, [{master_node, node1@localhost}]).

locks_2(Config) ->
    Nodes = [Node1, Node2] = [node1@localhost, node2@localhost],
    ?GH:bootstrap_remote(Node2),
    ?GH:bootstrap(Node1),
    ok = application:start(locks),
    {Pid, MRef} =
        spawn_monitor(
          fun () ->
                  {ok, Agt} = locks_agent:start(),
                  locks:change_flag(Agt, abort_on_deadlock, true),
                  locks:change_flag(Agt, await_nodes, true),
                  locks:lock(Agt, [1], write, Nodes, all)
          end),
    ok = rpc:call(Node2, application, start, [locks]),
    receive {'DOWN', MRef, _, _, _} -> ok end,
    ?G:exit_with(success).
