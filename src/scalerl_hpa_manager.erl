%%%-------------------------------------------------------------------
%%% @author $AUTHOR
%%% @copyright 2020 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(scalerl_hpa_manager).
-include_lib("kernel/include/logger.hrl").
-behaviour(gen_server).

%% API functions
-export([start_link/1]).
-export([update_hpa/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {api}).

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

update_hpa(Namespace, Name, {Min, Max}) ->
    ?LOG_INFO(#{what => "called update_hpa"}),
    gen_server:call(?MODULE, {update_hpa, Namespace, Name, {Min, Max}}).

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
    ?LOG_INFO(#{msg => "HPA manager starting"}),
    {ok, #state{api=API}}.

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
handle_call({update_hpa, Namespace, Name, {Min, Max}},
            _From,
            State=#state{api=API}) ->
    MaxUpdate = Max * 2,
    MinUpdate = lists:max([1, math:floor(Min * 0.75)]),
    ?LOG_INFO(#{what => "HPA manager should update HPA",
                namespace => Namespace,
                name => Name,
                min => Min,
                max => Max,
                min_update => MinUpdate,
                max_update => MaxUpdate}),
    HPAUpdateSpec = hpa_scale_update(Namespace, Name, {Min, Max}),
    RequestOps = [{"body", HPAUpdateSpec},
                  {"name", Name},
                  {"namespace", Namespace}],
    SwaggerlOps = [{content_type,
                   <<"application/strategic-merge-patch+json">>}],
    Resp = swaggerl:op(API,
        <<"patchAutoscalingV1NamespacedHorizontalPodAutoscaler">>,
        RequestOps,
        SwaggerlOps),
    ?LOG_DEBUG(#{what => "HPA Manager Swaggerl Op",
                namespace => Namespace,
                name => Name,
                response => Resp}),
    ?LOG_INFO(#{what => "HPA manager updated HPA",
                namespace => Namespace,
                name => Name,
                min => Min,
                max => Max,
                min_update => MinUpdate,
                max_update => MaxUpdate}),
    {reply, ok, State};
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
hpa_scale_update(Namespace, Name, {Min, Max}) ->
    #{<<"apiVersion">> => <<"autoscaling/v2beta2">>,
      <<"kind">> => <<"HorizontalPodAutoscaler">>,
      <<"metadata">> => #{
        <<"name">> => Name,
        <<"namespace">> => Namespace
      },
      <<"spec">> => #{
        <<"minReplicas">> => Min,
        <<"maxReplicas">> => Max
    }}.
