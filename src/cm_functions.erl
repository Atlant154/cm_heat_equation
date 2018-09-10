-module(cm_functions).
-export([function_of_heat_sources/2]).
-export([exact_solution/2]).
-export([get_tridiagonal_matrix/2]).
-export([get_free_part/3]).
-export([get_result_scatter/7]).
-export([write_file/4]).

-define(THERMAL_DIFFUSIVITY, 0.0021).
-define(TIME_LEFT_BOUND, 0.0).
-define(TIME_RIGHT_BOUND, 1.0).
-define(X_LEFT_BOUND, 0.0).
-define(X_RIGHT_BOUND, 1.0).

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

-spec get_init_free_part(float()) -> list().
get_init_free_part(H) ->
    get_init_free_part(?X_LEFT_BOUND, ?X_RIGHT_BOUND, H, []).

-spec get_init_free_part(float(), float(), float(), list()) -> list().
get_init_free_part(Iter, End, H, Result) when Iter > End + H/2 ->
    Result;
get_init_free_part(Iter, End, H, Result) ->
    % io:format("Iter: ~p. End: ~p. Iter + H = ~p.~n", [Iter, End, Iter + H]),
    get_init_free_part(Iter + H, End, H, [exact_solution(Iter, 0) | Result]).

-spec get_tridiagonal_matrix(float(), float()) -> {list(), list(), list()}.
get_tridiagonal_matrix(Tau, H) ->
    A_i = (?THERMAL_DIFFUSIVITY * Tau) / math:pow(H, 2),
    AC = [A_i || _ <- lists:seq(0, erlang:round((?X_RIGHT_BOUND - ?X_LEFT_BOUND) / H) - 1)],
    B = [(-1) * (A_i + 2) || _ <- lists:seq(0, erlang:round((?X_RIGHT_BOUND - ?X_LEFT_BOUND) / H))],
    {AC, B, AC}.

get_result_scatter(A, B, C, CalcPids, CalcRef, Tau, H) ->
    get_result_scatter(?TIME_LEFT_BOUND, ?TIME_RIGHT_BOUND, A, B, C, CalcPids, CalcRef, [], [], Tau, H).

get_result_scatter(TimeLeftBound,
                   TimeRightBound,
                   _A, _B, _C, _CalcPids, _CalcRef, _PreviousLayer,
                   Result, _Tau, _H) when TimeLeftBound > TimeRightBound ->
                       Result;
get_result_scatter(Left, Right, A, B, C, Pids, Ref, PrevLayer, Result, Tau, H) ->
    Free = get_free_part(Left, H, PrevLayer),
    % io:format("Free: ~p~n", [Free]),
    LocalResult = cm_tdma_functions:tridiagonal_matrix_algorythm(A, B, C, Free),
    get_result_scatter(Left + Tau, Right, A, B, C, Pids, Ref, LocalResult, [LocalResult | Result], Tau, H).

write_file(Path, Result, Tau, H) ->
    X = [?X_LEFT_BOUND, ?X_RIGHT_BOUND + H, H],
    Y = [?TIME_LEFT_BOUND, ?TIME_RIGHT_BOUND + Tau, Tau],
    Msg = [X, Y, lists:reverse(Result)],
    file:write_file(Path, io_lib:write(Msg)).