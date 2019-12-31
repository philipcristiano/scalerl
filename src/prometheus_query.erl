-module(prometheus_query).

-include_lib("kernel/include/logger.hrl").
-export([hpa_size/2]).

hpa_size(Namespace, Name) ->
    MaxQuery = lists:flatten(["max(kube_hpa_status_current_replicas{hpa=\"",
                              binary:bin_to_list(Name),
                              "\", namespace=\"",
                              binary:bin_to_list(Namespace),
                              "\"})"]),
    MinQuery = lists:flatten(["min(kube_hpa_status_current_replicas{hpa=\"",
                              binary:bin_to_list(Name),
                              "\", namespace=\"",
                              binary:bin_to_list(Namespace),
                              "\"})"]),
    MaxResp = query_instant("http://prometheus.stratobuilder.com",
      [{query, MaxQuery}]),

    ?LOG_INFO(#{what => "Prometheus Response",
                max_query => MaxResp}),

    MaxData = maps:get(<<"data">>, MaxResp),
    MaxResult = maps:get(<<"result">>, MaxData),
    [MaxItem] = MaxResult,
    MaxElement = maps:get(<<"value">>, MaxItem),
    [_MaxTime, MaxValue] = MaxElement,


    MinResp = query_instant("http://prometheus.stratobuilder.com",
      [{query, MinQuery}]),

    MinData = maps:get(<<"data">>, MinResp),
    MinResult = maps:get(<<"result">>, MinData),
    [MinItem] = MinResult,
    MinElement = maps:get(<<"value">>, MinItem),
    [_MinTime, MinValue] = MinElement,


    ?LOG_INFO(#{what => "Prometheus Metrics",
                max_query => MaxQuery,
                max_data => MaxData,
                min_query => MinQuery,
                min_data => MinData,
                namespace => Namespace,
                name => Name}),
    {ok, {MinValue, MaxValue}}.


query_instant(Host, Ops) ->
    Path = Host ++ "/api/v1/query",
    Headers = [],
    ReqBody = {form, Ops},
    ?LOG_DEBUG(#{msg => "Prometheus request",
                 path => Path,
                 body => ReqBody}),
    {ok, Code, _Headers, ReqRef} = hackney:request(
      post,
      Path,
      Headers,
      ReqBody
    ),
    {ok, RespBody} = hackney:body(ReqRef),
    ?LOG_DEBUG(#{msg => "Prometheus response",
                code => Code,
                body => RespBody}),
    Data = jsx:decode(RespBody, [return_maps]),
    Data.
