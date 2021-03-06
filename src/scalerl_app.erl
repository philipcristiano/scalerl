-module(scalerl_app).
-behaviour(application).
-include_lib("kernel/include/logger.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ?LOG_INFO(#{msg=>"starting scalerl"}),
    scalerl_sup:start_link().

stop(_State) ->
    ok.
