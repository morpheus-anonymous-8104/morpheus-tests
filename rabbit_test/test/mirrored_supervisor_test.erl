-module(mirrored_supervisor_test).

-compile(export_all).

-define(MS,     mirrored_supervisor).
-define(HELPER, mirrored_supervisor_test_helper).
-define(SERVER, mirrored_supervisor_test_gs).

-define(config(Key, Config), proplists:get_value(Key, Config)).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {timeout, 120, ?_test( test_entry() )}.

-define(T, morpheus_test_lib).
-define(GH, morpheus_guest_helper).
-define(G, morpheus_guest).

try_getenv(Name, Handler, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        S -> Handler(S)
    end.

test_entry() ->
    os:cmd("rm -r rabbit_data"), 
    ?T:start_morpheus_test(mirrored_supervisor_test, leave_while_joining, [{priv_dir, "rabbit_data"}]).

init_test(Config) ->
    ok = application:set_env(mnesia, dir, proplists:get_value(priv_dir, Config)),
    ok = application:start(mnesia),
    lists:foreach(
      fun ({Tab, TabDef}) ->
              TabDef1 = proplists:delete(match, TabDef),
              case mnesia:create_table(Tab, TabDef1) of
                  {atomic, ok} ->
                      ok;
                  {aborted, Reason} ->
                      throw({error,
                          {table_creation_failed, Tab, TabDef1, Reason}})
              end
      end, mirrored_supervisor:table_definitions()),
    Config.

%% ms-1
leave_while_joining(Config) ->
    ?GH:bootstrap(),
    init_test(Config),
    passed =
        ?HELPER:with_sups(
           fun ([A, B]) ->
                   {ok, _} = ?MS:start_child(a, ?HELPER:childspec(worker)),
                   {_, M} = spawn_monitor(
                              fun() ->
                                      io:format(user, "killing sup b~n", []),
                                      gen_server:stop(B),
                                      io:format(user ,"killed sup b~n", [])
                             end),
                  io:format(user, "joining sup c~n", []),
                  {ok, C} = ?HELPER:start_sup(c),
                  io:format(user, "joined sup c joined~n", []),
                  gen_server:stop(c),
                  receive {'DOWN', M, _, _, _} -> ok end,
                  ok
          end, [a, b]),
    ok = application:stop(mnesia),
    ?G:exit_with(success).
