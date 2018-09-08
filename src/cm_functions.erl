-module(cm_functions).
-export([tridiagonal_matrix_algorythm/4]).
-export([get_calculation_list/1]).
-export([test/1]).

-record(calculation, {
                        time :: float(),
                        list_of_results :: list()
}).

%% Tridiagonal matrix algorythm.

%% API:
-spec tridiagonal_matrix_algorythm(list(), list(), list(), list()) -> list().
tridiagonal_matrix_algorythm(Below, Main, Above, Free) ->
    {P, [QCurrent | QTail]} = get_tdma_coef(Below, Main, Above, Free),
    get_tdma_result(P, QTail, [QCurrent]).

%% TDMA coefficient(forward sweep):
-spec get_tdma_coef(list(), list(), list(), list()) -> {list(), list()}.
get_tdma_coef(Below, [MH | Main], [AH | Above], [FH | Free]) ->
    get_tdma_coef([(-1) * (AH / MH)], [FH / MH], Below, Main, Above, Free).

get_tdma_coef([PCurrent | _PTail] = P, [QCurrent | _QTail] = Q, [Below], [Main], [], [Free]) ->
    {P, [(Free - Below * QCurrent) / (Main + Below * PCurrent) | Q]};
get_tdma_coef([PCurrent | _PTail] = P, 
              [QCurrent | _QTail] = Q, 
              [BCurrent | BTail],
              [MCurrent | MTail],
              [ACurrent | ATail],
              [FCurrent | FTail]) ->
    get_tdma_coef([(-1) * (ACurrent / (MCurrent + BCurrent * PCurrent)) | P],
                  [(FCurrent - BCurrent * QCurrent) / (MCurrent + BCurrent * PCurrent) | Q],
                  BTail,
                  MTail,
                  ATail,
                  FTail).

%% Solution(coefficient substitution):
-spec get_tdma_result(list(), list(), list()) -> list().
get_tdma_result([], _Q, Results) ->
    Results;
get_tdma_result([P | PTail], [Q | QTail], [X | _XTail] = Results) ->
    X_i = P * X + Q,
    get_tdma_result(PTail, QTail, [X_i | Results]).

%% Heat equation.

-spec get_calculation_list(float()) -> list().
get_calculation_list(Tau) ->
    get_calculation_list(0.0, 1, [], Tau).

get_calculation_list(LeftBound, 1, CalcList, _Tau) when LeftBound > 1 ->
    CalcList;
get_calculation_list(LeftBound, 1, CalcList, Tau) ->
    get_calculation_list(LeftBound + Tau, 1, lists:append([CalcList, [LeftBound]]), Tau).

%% Tests.

test(NumOfMeasurement) ->
    io:format("TDMA test. Result = ~p mcs.~n", [tridiagonal_matrix_algorythm_test(0, NumOfMeasurement, 0)]).

tridiagonal_matrix_algorythm_test(Iter, Iter, Val) ->
    Val / Iter;
tridiagonal_matrix_algorythm_test(TIter, Iter, Val) ->
    Below = [-3, -5, -6, -5],
    Abowe = [-1, -1, 2, -4],
    Main = [2, 8, 12, 18, 10],
    Free = [-25, 72, -69, -156, 20],
    {Time, _CalcRes} = timer:tc(cm_functions, tridiagonal_matrix_algorythm, [Below, Main, Abowe, Free]),
    % io:format("Calc result = ~p.~n", [CalcRes]),
    tridiagonal_matrix_algorythm_test(TIter + 1, Iter, Val + Time).