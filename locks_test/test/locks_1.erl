-module(locks_1).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

-define(T, morpheus_test_lib).
-define(G, morpheus_guest).
-define(GH, morpheus_guest_helper).

test_entry() ->
    ?T:start_morpheus_test(locks_1, locks_1, []).

locks_1(Config) ->
    ?GH:bootstrap(),
    ok = application:start(locks),
    Me = self(),
    lists:foreach(fun (Id) -> spawn(fun () -> locks_1_client(Me, Id) end) end, lists:seq(1, 3)),
    lists:foreach(fun (Id) -> receive {Id, ok} -> io:format("~p~n", [{Id, finished}]) end end, lists:seq(1, 3)),
    ?G:exit_with(success).

locks_1_client(CtrlProc, Id) ->
    {ok, Agt} = locks_agent:start(),
    LockOrder = case Id of
                    1 -> [[1], [2], [3]];
                    2 -> [[2], [3], [1]];
                    3 -> [[3], [1], [2]]
                end,
    lists:foreach(fun (Lock) -> io:format("~p(~p) ~p lock ~p call ~p~n",
                                          [self(), Agt, Id, Lock, locks:lock(Agt, Lock, write)])
                  end, LockOrder),
    locks:end_transaction(Agt),
    CtrlProc ! {Id, ok}.
