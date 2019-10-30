-module(scalerl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    API = kuberlnetes:load(),
    Procs = [
        #{id    => scalerl_deployment_watcher,
          start => {scalerl_deployment_watcher, start_link, [API]}
        },
        #{id    => scalerl_hpa_watcher,
          start => {scalerl_hpa_watcher, start_link, [API]}
        }
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
