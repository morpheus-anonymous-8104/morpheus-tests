-module(morpheus_test_lib).

-export([start_morpheus_test/3]).

-include_lib("morpheus/include/morpheus.hrl").

-define(config(Key, Data), proplists:get_value(Key, Data)).

try_getenv(Name, Handler, Default) ->
    case os:getenv(Name) of
        false ->
            Default;
        S -> Handler(S)
    end.

start_morpheus_test(Module, Function, InitConfig) ->
    Config = dict:to_list(
               dict:merge(fun (_K, V1, _V2) -> V1 end, dict:from_list(InitConfig),
                          dict:from_list(
                            [ {sched, try_getenv("SCHED", fun list_to_atom/1, basicpos)}
                            , {repeat, try_getenv("REPEAT", fun list_to_integer/1, 1)}
                            , {pred, try_getenv("PRED", fun list_to_atom/1, no)}
                            , {pred_skip, try_getenv("PRED_SKIP", fun ("") -> false; (_) -> true end, false)}
                            , {acc_filename, try_getenv("ACC_FILENAME", fun (I) -> I end, "acc.dat")}
                            , {master_node, master@localhost}
                            , {clock_limit, 600000}
                              %% A timestamp randomly picked, 10/09/2018 @ 5:12pm (UTC)
                            , {clock_offset, 1539105131938}
                            ]))
              ),
    Pred = ?config(pred, Config),
    Tracer =
        case Pred of
            no -> undefined;
            _ ->
                {ok, _Tracer} =
                    morpheus_tracer:start_link(
                      [ {acc_filename, ?config(acc_filename, Config)}
                      , {find_races, true}
                      , {extra_opts,
                         maps:from_list(
                           [ {verbose_race_info, true}
                           , {verbose_racing_prediction_stat, true}
                           ]
                          )}
                      ]
                      ++ case Pred of
                             path ->
                                 [ {path_coverage, true}
                                 , {to_predict, true}
                                 , {predict_by, path}
                                 ];
                             ploc ->
                                 [ {line_coverage, true}
                                 , {to_predict, true}
                                 , {predict_by, ploc}
                                 ]
                         end
                     ),
                _Tracer
        end,
    MConfig =
        [ monitor
        , { fd_opts
          , [ { scheduler
              , {?config(sched, Config), []} }
            , verbose_final ] }
        , {node, ?config(master_node, Config)}
        , {clock_limit, ?config(clock_limit, Config)}
        , {throttle_control, {10000, 10}}
        , {clock_offset, ?config(clock_offset, Config)}         
        ]
        ++ case Tracer of
               undefined -> [];
               _ -> [{tracer_pid, Tracer}]
           end
        ++ case Pred of
               no -> [];
               _ -> [{use_prediction, case ?config(pred_skip, Config) of true -> skip; false -> true end}]
           end
        ,
    {_Ctl, MRef} = morpheus_sandbox:start(
                    Module, Function, [Config],
                    MConfig),
    receive
        {'DOWN', MRef, _, _, Reason} ->
            case Tracer of
                undefined -> ok;
                _ -> morpheus_tracer:stop(Tracer)
            end,
            success = Reason
    end,
    ok.
