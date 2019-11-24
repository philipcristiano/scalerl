%%%-------------------------------------------------------------------
%%% @author philipcristiano
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(scalerl_deployment_watcher).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {api, pid}).

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
    ?LOG_INFO(#{msg => "Deployment watcher starting"}),
    Self = self(),
    Callback = fun({Type, Obj}) -> Self ! {kubewatch, Type, Obj} end,
    Pid = kuberlnetes:spawn_watch(
        Callback, API, "listAppsV1DeploymentForAllNamespaces", []),
    {ok, #state{api=API, pid=Pid}}.

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
    ScalerlEnabled = maps:get(<<"scalerl">>, Annotations, "disable"),
    State1 = ensure_scalerl_hpa(Metadata, ScalerlEnabled, State),
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

ensure_scalerl_hpa(Metadata, <<"enable">>, State = #state{api=API}) ->
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),
    ?LOG_INFO(#{what => "deployment should be enabled for Scalerl",
                namespace => Namespace,
                deployment => Name}),
    HPADoc = hpa(Name, Metadata),
    _Resp = swaggerl:op(
        API,
        <<"createAutoscalingV2beta2NamespacedHorizontalPodAutoscaler">>,
        [{"body", HPADoc},
         {"namespace", Namespace}]),
    ?LOG_INFO(#{what => "created HPA",
                namespace => Namespace,
                deployment => Name}),
    State;
ensure_scalerl_hpa(Metadata, _, State) ->
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),
    ?LOG_INFO(#{what => "deployment not enabled",
               namespace => Namespace,
               depoyment => Name}),
    State.


hpa(Deployment, _Metadata) ->
    #{<<"apiVersion">> => <<"autoscaling/v2beta2">>,
      <<"kind">> => <<"HorizontalPodAutoscaler">>,
      <<"metadata">> => #{
        <<"name">> => Deployment,
        <<"annotations">> => #{
            <<"scalerl">> => <<"enable">>
        }
      },
      <<"spec">> => #{
        <<"scaleTargetRef">> => #{
          <<"apiVersion">> => <<"apps/v1">>,
          <<"kind">> => <<"Deployment">>,
          <<"name">> => Deployment
      },
        <<"minReplicas">> => 1,
        <<"maxReplicas">> => 10,
        <<"metrics">> => [
          #{<<"type">> => <<"Resource">>,
            <<"resource">> => #{
              <<"name">> => <<"cpu">>,
              <<"target">> => #{
                <<"type">> => <<"Utilization">>,
                <<"averageUtilization">> => 50
              }
           }
       }]
    }}.
