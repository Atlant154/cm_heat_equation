-module(cm_benchmarks).
-export([tdma_test/1]).
-export([heat_test/1]).

%% TDMA benchmark:
tdma_test(NumOfMeasurement) ->
    io:format("TDMA benchmark. Result = ~p µs.~n", [tridiagonal_matrix_algorythm_test(0, NumOfMeasurement, 0)]).

tridiagonal_matrix_algorythm_test(Iter, Iter, Val) ->
    Val / Iter;
tridiagonal_matrix_algorythm_test(TIter, Iter, Val) ->
    Below = [rand:uniform(100) || _ <- lists:seq(1, 1000)],
    Abowe = [rand:uniform(100) || _ <- lists:seq(1, 1000)],
    Main = [rand:uniform(100) || _ <- lists:seq(1, 1001)],
    Free = [rand:uniform(100) || _ <- lists:seq(1, 1001)],
    {Time, _CalcRes} = timer:tc(cm_tdma_functions, tridiagonal_matrix_algorythm, [Below, Main, Abowe, Free]),
    tridiagonal_matrix_algorythm_test(TIter + 1, Iter, Val + Time).

%% Heat equation calculation benchmark:
heat_test(NumOfMeasurement) ->
    io:format("Heat equation calculation benchmark. Result = ~p µs.~n", [cm_heat(0, NumOfMeasurement, 0)]).

cm_heat(Iter, Iter, Val) ->
    Val / Iter;
cm_heat(TIter, Iter, Val) ->
    {Time, _CalcRes} = timer:tc(postman_srv, calculate, [20, 1024]),
    cm_heat(TIter + 1, Iter, Val + Time).