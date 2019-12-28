%%%-------------------------------------------------------------------
%%% @author philipcristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(scalerl_hpa_watcher).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API functions
-export([start_link/1]).
-export([update_hpa/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {api, pid, hpa_timers}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(API) ->
    ?LOG_INFO(#{msg => "HPA watcher starting"}),
    Self = self(),
    Callback = fun({Type, Obj}) -> Self ! {kubewatch, Type, Obj} end,
    Pid = kuberlnetes:spawn_watch(
      Callback,
      API,
      "listAutoscalingV1HorizontalPodAutoscalerForAllNamespaces",
      []
    ),
    HPATimers = maps:new(),
    {ok, #state{api=API, pid=Pid, hpa_timers=HPATimers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({kubewatch, _Type, Object}, State) ->
    Metadata = maps:get(<<"metadata">>, Object),

    Annotations = maps:get(<<"annotations">>, Metadata, #{}),
    ScalerlEnabled = maps:get(<<"scalerl">>, Annotations, "default_disabled"),
    State1 = watch_hpa(Metadata, ScalerlEnabled, State),

    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

watch_hpa(Metadata, <<"enable">>, State = #state{hpa_timers=HPATimers}) ->
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),
    ?LOG_INFO(#{what => "HPA should be watched",
                namespace => Namespace,
                name => Name}),
    HPATimers1 = ensure_timer(Namespace, Name, HPATimers),
    State1 = State#state{hpa_timers=HPATimers1},
    State1;
watch_hpa(Metadata, Setting, State = #state{hpa_timers=HPATimers}) ->
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),
    ?LOG_INFO(#{what => "HPA should not be watched",
                namespace => Namespace,
                name => Name,
                scalerl_setting => Setting}),
    HPATimers1 = ensure_timer_removed(Namespace, Name, HPATimers),
    State1 = State#state{hpa_timers=HPATimers1},
    State1.

ensure_timer(Namespace, Name, HPATimers) ->
    PossibleTRef = maps:get({Namespace, Name}, HPATimers, undefined),
    create_timer(Namespace, Name, HPATimers, PossibleTRef).

ensure_timer_removed(Namespace, Name, HPATimers) ->
    PossibleTRef = maps:get({Namespace, Name}, HPATimers, undefined),
    remove_timer(Namespace, Name, HPATimers, PossibleTRef).

create_timer(Namespace, Name, HPATimers, undefined) ->
    ?LOG_DEBUG(#{msg=>"Creating HPA Timer",
                 namespace => Namespace,
                 name => Name}),
    Seconds = 5,
    Interval = Seconds * 1000,
    TRef = timer:apply_interval(Interval,
                                scalerl_hpa_watcher,
                                update_hpa,
                                [{Namespace, Name}]),
    maps:put({Namespace, Name}, TRef, HPATimers);
create_timer(_Namespace, _Name, HPATimers, _) ->
    HPATimers.

remove_timer(_Namespace, _Name, HPATimers, undefined) ->
    HPATimers;
remove_timer(Namespace, Name, HPATimers, TRef) ->
    ?LOG_DEBUG(#{msg=>"Removing HPA Timer",
                 namespace => Namespace,
                 name => Name}),
    {ok, cancel} = timer:cancel(TRef),
    maps:remove({Namespace, Name}, HPATimers).

update_hpa({Namespace, Name}) ->
    ?LOG_INFO(#{msg=> "Apply interval update HPA",
                namespace => Namespace,
                name => Name}),
    MaxQuery = lists:flatten(["max(kube_hpa_status_current_replicas{hpa=\"",
                              Name,
                              "\", namespace=\"",
                              Namespace,
                              "\"})"]),
    Data = prometheus_query:query("http://prometheus.stratobuilder.com",
      [{query, MaxQuery}]),
    ?LOG_INFO(#{what => "Prometheus Metrics",
                data => Data,
                name => Name}),
    ok.
