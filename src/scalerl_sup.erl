-module(scalerl_sup).
-behaviour(supervisor).
-include_lib("kernel/include/logger.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?LOG_INFO(#{msg=>"Getting Kubernetes API Credentials"}),
    Operations = [
        <<"createAutoscalingV2beta2NamespacedHorizontalPodAutoscaler">>,
        <<"listAppsV1DeploymentForAllNamespaces">>,
        <<"listAutoscalingV1HorizontalPodAutoscalerForAllNamespaces">>
    ],

    API = kuberlnetes:load([{operations, Operations}]),
    ?LOG_INFO(#{msg=>"Got Kubernetes API Credentials"}),
    ?LOG_INFO(#{api_options=>swaggerl:operations(API)}),
    Procs = [
        #{id    => scalerl_deployment_watcher,
          start => {scalerl_deployment_watcher, start_link, [API]}
        },
        #{id    => scalerl_hpa_watcher,
          start => {scalerl_hpa_watcher, start_link, [API]}
        }
    ],
  {ok, {{one_for_one, 1, 5}, Procs}}.
