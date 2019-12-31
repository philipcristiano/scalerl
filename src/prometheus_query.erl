-module(prometheus_query).

-include_lib("kernel/include/logger.hrl").
-export([hpa_size/2]).

hpa_size(Namespace, Name) ->
    DaysOfData = 1,
    Now = os:system_time(seconds),
    Past = Now - (DaysOfData * 86400),

    Query = lists:flatten(["kube_hpa_status_current_replicas{hpa=\"",
                              binary:bin_to_list(Name),
                              "\", namespace=\"",
                              binary:bin_to_list(Namespace),
                              "\"}"]),
    Resp = query_range("http://prometheus.stratobuilder.com",
      [{query, Query},
       {step, "5m"},
       {start, Past},
       {'end', Now}]),

    ?LOG_INFO(#{what => "Prometheus Response",
                query => Resp}),

    Data = maps:get(<<"data">>, Resp),
    Result = maps:get(<<"result">>, Data),
    [Item] = Result,
    Elements = maps:get(<<"values">>, Item),
    % TODO: process elements
    Values = lists:map(fun int_values_from_data/1, Elements),
    Max = lists:max(Values),
    Min = lists:min(Values),

    ?LOG_INFO(#{what => "Prometheus Metrics",
                query => Query,
                namespace => Namespace,
                name => Name}),
    {ok, {Min, Max}}.

int_values_from_data([_Timestamp, Value]) ->
      {Int, _Rest} = string:to_integer(erlang:binary_to_list(Value)),
      Int.


% query_instant(Host, Ops) ->
%     Path = Host ++ "/api/v1/query",
%     Headers = [],
%     ReqBody = {form, Ops},
%     ?LOG_DEBUG(#{msg => "Prometheus request",
%                  path => Path,
%                  body => ReqBody}),
%     {ok, Code, _Headers, ReqRef} = hackney:request(
%       post,
%       Path,
%       Headers,
%       ReqBody
%     ),
%     {ok, RespBody} = hackney:body(ReqRef),
%     ?LOG_DEBUG(#{msg => "Prometheus response",
%                 code => Code,
%                 body => RespBody}),
%     Data = jsx:decode(RespBody, [return_maps]),
%     Data.

query_range(Host, Ops) ->
    Path = Host ++ "/api/v1/query_range",
    Headers = [],
    ReqBody = {form, Ops},
    ?LOG_DEBUG(#{msg => "Prometheus range request",
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
