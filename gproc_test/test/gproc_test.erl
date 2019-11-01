-module(gproc_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("morpheus/include/morpheus.hrl").

all_test_() ->
    {timeout, 3600, ?_test( test_entry() )}.

-define(T, morpheus_test_lib).
-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).

test_entry() ->
    InitConfig0 = [ {testcase, list_to_atom(os:getenv("TESTCASE"))}
                 , {nodes, [node1@localhost, node2@localhost, node3@localhost]}],
    InitConfig = InitConfig0 ++ [{reset_gproc,
                                  case proplists:get_value(testcase, InitConfig0) of
                                      t_master_dies -> false;
                                      _ -> true
                                  end}],
    %% Clean up stale state files
    lists:foreach(
      fun (N) -> os:cmd(lists:flatten(io_lib:format("rm gproc_dist_~s", [N]))) end,
      proplists:get_value(nodes, InitConfig)),
    ?T:start_morpheus_test(gproc_test, test_inner_entry, InitConfig),
    ok.

test_inner_entry(Config) ->
    Ns = proplists:get_value(nodes, Config),
    lists:foreach(fun (N) -> ?GH:bootstrap_remote(N) end, Ns),
    ?GH:bootstrap(proplists:get_value(master_node, Config)),
    {[ok,ok,ok],[]} = rpc:multicall(Ns, application, set_env, [gproc, gproc_dist, Ns]),
    case proplists:get_value(reset_gproc, Config) of
        true -> {[ok,ok,ok],[]} = rpc:multicall(Ns, application, ensure_started, [gproc]);
        false -> lists:foreach(fun (N) -> ok = rpc:call(N, application, ensure_started, [gproc]),
                                          timer:sleep(250) end, Ns)
    end,
    apply(gproc_test, proplists:get_value(testcase, Config), [Ns]),
    timer:sleep(1000),
    case proplists:get_value(reset_gproc, Config) of
        true -> lists:foreach(fun (N) -> ok = rpc:call(N, application, stop, [gproc]) end, Ns);
        false -> ok
    end,
    ?G:exit_with(success).

%% Copied from the original gproc/test/gproc_test.erl, except t_master_dies.

-define(T_NAME, {n, g, {?MODULE, ?LINE, os:timestamp()}}).
-define(T_KVL, [{foo, "foo"}, {bar, "bar"}]).
-define(T_COUNTER, {c, g, {?MODULE, ?LINE}}).
-define(T_RESOURCE, {r, g, {?MODULE, ?LINE}}).
-define(T_PROP, {p, g, ?MODULE}).

%% gproc-1
t_simple_reg([H|_] = Ns) ->
    Name = ?T_NAME,
    P = t_spawn_reg(H, Name),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, P)),
    %% io:format(user, "start testing before unreg~n", []),
    %% ?G:call_ctl({nodelay, {set_fd_guidance, [0]}}),
    ?assertMatch(true, t_call(P, {apply, gproc, unreg, [Name]})),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, undefined)),
    ?assertMatch(ok, t_call(P, die)).

%% gproc-2
t_simple_ensure_other([A, B|_] = Ns) ->
    Name = ?T_NAME,
    P1 = t_spawn(A),
    P2 = t_spawn(B),
    ?assertMatch(true, t_call(P1, {apply, gproc, reg_other, [Name, P2]})),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, P2)),
    ?assertMatch(
       updated, t_call(
                  P1, {apply, gproc, ensure_reg_other, [Name, P2, new_val]})),
    ?assertMatch(ok, t_read_everywhere(Name, P2, Ns, new_val)),
    ?assertMatch(true, t_call(P1, {apply, gproc, unreg_other, [Name, P2]})),
    ?assertMatch(ok, t_lookup_everywhere(Name, Ns, undefined)),
    ?assertMatch(ok, t_call(P1, die)),
    ?assertMatch(ok, t_call(P2, die)).

%% gproc-3
t_master_dies([A,B,C] = Ns) ->
    Na = ?T_NAME,
    Nb = ?T_NAME,
    Nc = ?T_NAME,
    Pa = t_spawn_reg(A, Na),
    Pb = t_spawn_reg(B, Nb),
    Pc = t_spawn_reg(C, Nc),
    L = rpc:call(A, gproc_dist, get_leader, []),
    ?assertMatch(ok, t_lookup_everywhere(Na, Ns, Pa)),
    ?assertMatch(ok, t_lookup_everywhere(Nb, Ns, Pb)),
    ?assertMatch(ok, t_lookup_everywhere(Nc, Ns, Pc)),
    {Nl, Pl} = case L of
                   A -> {Na, Pa};
                   B -> {Nb, Pb};
                   C -> {Nc, Pc}
               end,
    ?assertMatch(true, rpc:call(A, gproc_dist, sync, [])),
    ?assertMatch(ok, rpc:call(L, application, stop, [gproc])),
    Names = [{Na,Pa}, {Nb,Pb}, {Nc,Pc}] -- [{Nl, Pl}],
    RestNs = Ns -- [L],
    %% ?assertMatch(true, rpc:call(hd(RestNs), gproc_dist, sync, [])),
    ?assertMatch(true, try_sync(hd(RestNs), RestNs)),
    ?assertMatch(ok, t_lookup_everywhere(Nl, RestNs, undefined)),
    [?assertMatch(ok, t_lookup_everywhere(Nx, RestNs, Px))
     || {Nx, Px} <- Names],
    ok.

try_sync(N, Ns) ->
    io:format(user, "call ~p:gproc_dist:sync~n", [N]),
    case rpc:call(N, gproc_dist, sync, []) of
        {badrpc, _} = Err ->
            ?debugFmt(
               "Error in gproc_dist:sync() (~p):~n"
               "  ~p~n"
               "Status = ~p~n",
               [Err, N,
                {Ns, rpc:multicall([N|Ns], sys, get_status, [gproc_dist])}]),
            Err;
        true ->
            true
    end.

t_sleep() ->
    timer:sleep(500).

t_lookup_everywhere(Key, Nodes, Exp) ->
    true = rpc:call(hd(Nodes), gproc_dist, sync, []),
    t_lookup_everywhere(Key, Nodes, Exp, 10).

t_lookup_everywhere(Key, _, Exp, 0) ->
    {lookup_failed, Key, Exp};
t_lookup_everywhere(Key, Nodes, Exp, I) ->
    Expected = [{N, Exp} || N <- Nodes],
    Found = [{N,rpc:call(N, gproc, where, [Key])} || N <- Nodes],
    if Expected =/= Found ->
	    ?debugFmt("lookup ~p failed~n"
		      "(Expected: ~p;~n"
		      " Found   : ~p)~n"
                      "status = ~p:~p, retrying...~n",
		      [Key, Expected, Found, rpc:multicall(Nodes, sys, get_status, [gproc_dist]), rpc:multicall(Nodes, sys, get_status, [gproc])]),
	    t_sleep(),
	    t_lookup_everywhere(Key, Nodes, Exp, I-1);
       true ->
	    ok
    end.

t_read_everywhere(Key, Pid, Nodes, Exp) ->
    true = rpc:call(hd(Nodes), gproc_dist, sync, []),
    t_read_everywhere(Key, Pid, Nodes, Exp, 3).

t_read_everywhere(Key, _, _, Exp, 0) ->
    {read_failed, Key, Exp};
t_read_everywhere(Key, Pid, Nodes, Exp, I) ->
    Expected = [{N, Exp} || N <- Nodes],
    Found = [{N, read_result(rpc:call(N, gproc, get_value, [Key, Pid]))}
	     || N <- Nodes],
    if Expected =/= Found ->
	    ?debugFmt("read ~p failed~n"
		      "(Expected: ~p;~n"
		      " Found   : ~p), retrying...~n",
		      [{Key, Pid}, Expected, Found]),
	    t_sleep(),
	    t_read_everywhere(Key, Pid, Nodes, Exp, I-1);
       true ->
	    ok
    end.

read_result({badrpc, {'EXIT', {badarg, _}}}) -> badarg;
read_result(R) -> R.

t_spawn(Node) -> gproc_test_lib:t_spawn(Node).
t_spawn(Node, Selective) -> gproc_test_lib:t_spawn(Node, Selective).
t_spawn_mreg(Node, KVL) -> gproc_test_lib:t_spawn_mreg(Node, KVL).
t_spawn_reg(Node, N) -> gproc_test_lib:t_spawn_reg(Node, N).
t_spawn_reg(Node, N, V) -> gproc_test_lib:t_spawn_reg(Node, N, V).
t_spawn_reg(Node, N, V, As) -> gproc_test_lib:t_spawn_reg(Node, N, V, As).
t_spawn_reg_shared(Node, N, V) -> gproc_test_lib:t_spawn_reg_shared(Node, N, V).
got_msg(P) -> gproc_test_lib:got_msg(P).
got_msg(P, Tag) -> gproc_test_lib:got_msg(P, Tag).
no_msg(P, Timeout) -> gproc_test_lib:no_msg(P, Timeout).

t_call(P, Req) ->
    gproc_test_lib:t_call(P, Req).
