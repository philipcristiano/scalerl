-module(scalerl_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([run/1]).

start(_Type, _Args) ->
    KAPI = kuberlnetes:load(),
    Ops = swaggerl:operations(KAPI),
    io:format("~p~n", [Ops]),
    erlang:spawn_link(scalerl_app, run, [KAPI]),
	scalerl_sup:start_link().

stop(_State) ->
	ok.

run(KAPI) ->
    ok = kuberlnetes:watch(KAPI, {"listAppsV1DeploymentForAllNamespaces", "watchAppsV1DeploymentListForAllNamespaces"}).
