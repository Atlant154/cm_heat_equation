-module(cm_functions).
-export([tridiagonal_matrix_algorythm/4]).
-export([get_calculation_list/1]).
-export([get_free_part/3]).
-export([function_of_heat_sources/2]).
-export([test/1]).

-define(THERMAL_DIFFUSIVITY, 0.0021).
-define(TIME_LEFT_BOUND, 0).
-define(TIME_RIGHT_BOUND, 1).
-define(X_LEFT_BOUND, 0).
-define(X_RIGHT_BOUND, 1).

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

-spec exact_solution(float(), float()) -> float().
exact_solution(XCoord, Time) ->
    (-2) * math:pow(XCoord, 4) - 3 * math:pow(Time, 3) + 3 * math:pow(Time, 2) * XCoord + math:exp(XCoord).

-spec function_of_heat_sources(float(), float()) -> float().
function_of_heat_sources(XCoord, Time) ->
    math:pow(XCoord, 2) * (math:pow(XCoord, 2) - 12 * ?THERMAL_DIFFUSIVITY * Time) + math:exp(XCoord) *
    (XCoord * Time * (?THERMAL_DIFFUSIVITY * Time - 2) + ?THERMAL_DIFFUSIVITY * math:pow(Time, 2) +
    2 * Time).

-spec get_free_part(float(), float(), list()) -> list().
get_free_part(?TIME_LEFT_BOUND, H, _PreviousTimeLayerFree) ->
    get_init_free_part(H);
get_free_part(_Else, _H, PreviousTimeLayerFree) ->
    PreviousTimeLayerFree.

get_init_free_part(H) ->
    get_init_free_part(?X_LEFT_BOUND, ?X_RIGHT_BOUND, H, []).

get_init_free_part(Iter, End, _H, Result) when Iter > End->
    Result;
get_init_free_part(Iter, End, H, Result) ->
    get_init_free_part(Iter + H, End, H, [exact_solution(Iter, 0) | Result]).

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
    Below = [rand:uniform(100) || _ <- lists:seq(1, 1000)],
    Abowe = [rand:uniform(100) || _ <- lists:seq(1, 1000)],
    Main = [rand:uniform(100) || _ <- lists:seq(1, 1001)],
    Free = [rand:uniform(100) || _ <- lists:seq(1, 1001)],
    {Time, CalcRes} = timer:tc(cm_functions, tridiagonal_matrix_algorythm, [Below, Main, Abowe, Free]),
    io:format("Calc result = ~p.~n", [CalcRes]),
    tridiagonal_matrix_algorythm_test(TIter + 1, Iter, Val + Time).