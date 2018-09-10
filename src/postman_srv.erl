-module(postman_srv).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([calculate/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TIME_LEFT_BOUND, 0).
-define(TIME_RIGHT_BOUND, 1).
-define(X_LEFT_BOUND, 0).
-define(X_RIGHT_BOUND, 1).

-record(state, {
	available_cores :: integer(),
	priv :: string()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec calculate(integer(), integer()) -> ok.
calculate(NTau, NH) when is_integer(NTau) andalso is_integer(NH) andalso NTau >= 1 andalso NH >= 1 ->
	gen_server:call(?MODULE, {calculate, NTau, NH}).

%% gen_server.

init([]) ->
	{ok, #state{available_cores = erlang:system_info(schedulers_online), priv = code:priv_dir(cm_heat_equation)}}.

handle_call({calculate, NTau, NH}, _From, State = #state{available_cores = _Cores, priv = Priv}) ->
	Tau = (?TIME_RIGHT_BOUND - ?TIME_LEFT_BOUND) / NTau,
	H = (?X_RIGHT_BOUND - ?X_LEFT_BOUND) / NH,
	% CalculatorsList = get_tdma_calculators_list(Cores),
	{A, B, C} = cm_functions:get_tridiagonal_matrix(Tau, H),
	Result = cm_functions:get_result_scatter(A, B, C, [], make_ref(), Tau, H),
	cm_functions:write_file(Priv ++ "/result.txt", Result, Tau, H),
	{reply, Result, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% get_tdma_calculators_list(Cores) ->
% 	case cm_tdma_sup:children_pid_list() of
% 		[] ->
% 			start_tdma_calculators(Cores),
% 			cm_tdma_sup:children_pid_list();
% 		ChildrenList ->
% 			ChildrenList
% 	end.

% start_tdma_calculators(Cores) ->
% 	start_tdma_calculators(0, Cores).

% start_tdma_calculators(Cores, Cores) ->
% 	ok;
% start_tdma_calculators(Iter, Cores) ->
% 	supervisor:start_child(cm_tdma_sup, []),
% 	start_tdma_calculators(Iter + 1, Cores).