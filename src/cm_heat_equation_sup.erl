-module(cm_heat_equation_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Flags = #{
				strategy => one_for_all,
				intensity => 10,
				period => 1000
	},
	Childrens = [#{
					id => postman,
					start => {postman_srv, start_link, []},
					shutdown => infinity,
					restart => permanent,
					type => worker,
					modules => [postman_srv]
				}
			% ,
			% 	#{
			% 		id => tdma_sup,
			% 		start => {cm_tdma_sup, start_link, []},
			% 		shutdown => infinity,
			% 		restart => permanent,
			% 		type => supervisor,
			% 		modules => [cm_tdma_sup]
			% 	}
			],
	{ok, {Flags, Childrens}}.
