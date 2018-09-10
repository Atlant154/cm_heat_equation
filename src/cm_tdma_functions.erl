-module(cm_tdma_functions).
-export([tridiagonal_matrix_algorythm/4]).

-record(tdma_coef, {
                    p :: list(),
                    q :: list(),
                    below :: list(),
                    main :: list(),
                    above :: list(),
                    free :: list()
}).

%% API:
-spec tridiagonal_matrix_algorythm(list(), list(), list(), list()) -> list().
tridiagonal_matrix_algorythm(Below, Main, Above, Free) ->
    {P, [QCurrent | QTail]} = get_tdma_coef(Below, Main, Above, Free),
    get_tdma_result(P, QTail, [QCurrent]).

%% TDMA coefficient(forward sweep):
-spec get_tdma_coef(list(), list(), list(), list()) -> {list(), list()}.
get_tdma_coef(Below, [MH | Main], [AH | Above], [FH | Free]) ->
    InitPCoef = [(-1) * (AH / MH)],
    InitQCoef = [FH / MH],
    Msg = #tdma_coef{
                        p = InitPCoef,
                        q = InitQCoef,
                        below = Below,
                        main = Main,
                        above = Above,
                        free = Free

    },
    get_tdma_coef(Msg).

get_tdma_coef(#tdma_coef{above = []} = State) ->
    [QPenultCoef | _QTail] = State#tdma_coef.q,
    [PLastCoef | _PTail] = State#tdma_coef.p,
    [LastFreeElement] = State#tdma_coef.free,
    [LastMainElement] = State#tdma_coef.main,
    [LastBelowElement] = State#tdma_coef.below,
    QLastCoef = (LastFreeElement - LastBelowElement * QPenultCoef) / (LastMainElement + LastBelowElement * PLastCoef),
    {State#tdma_coef.p, [QLastCoef | State#tdma_coef.q]};
get_tdma_coef(State = #tdma_coef{}) ->
    [PLastCoef | _PTail] = State#tdma_coef.p,
    [QLastCoef | _QTail] = State#tdma_coef.q,
    [BelowLastCoef | BelowTail] = State#tdma_coef.below,
    [MainLastCoef | MainTail] = State#tdma_coef.main,
    [AboveLastCoef | AboveTail] = State#tdma_coef.above,
    [FreeLastCoef | FreeTail] = State#tdma_coef.free,
    PNewCoef = (-1) * (AboveLastCoef / (MainLastCoef + BelowLastCoef * PLastCoef)),
    QNewCoef = (FreeLastCoef - BelowLastCoef * QLastCoef) / (MainLastCoef + BelowLastCoef * PLastCoef),
    Msg = #tdma_coef{
                        p = [PNewCoef | State#tdma_coef.p],
                        q = [QNewCoef | State#tdma_coef.q],
                        below = BelowTail,
                        main = MainTail,
                        above = AboveTail,
                        free = FreeTail
                    },
    get_tdma_coef(Msg).


%% Solution(coefficient substitution):
-spec get_tdma_result(list(), list(), list()) -> list().
get_tdma_result([], _Q, Results) ->
    Results;
get_tdma_result([P | PTail], [Q | QTail], [X | _XTail] = Results) ->
    X_i = P * X + Q,
    get_tdma_result(PTail, QTail, [X_i | Results]).