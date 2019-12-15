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
    ?LOG_INFO(#{msg => "HPA watcher starting"}),
    Self = self(),
    Callback = fun({Type, Obj}) -> Self ! {kubewatch, Type, Obj} end,
    Pid = kuberlnetes:spawn_watch(
      Callback,
      API,
      "listAutoscalingV1HorizontalPodAutoscalerForAllNamespaces",
      []
    ),
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

watch_hpa(Metadata, <<"enable">>, State = #state{}) ->
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),
    ?LOG_INFO(#{what => "HPA should be watched",
                namespace => Namespace,
                name => Name}),
    State;
watch_hpa(Metadata, _, State = #state{}) ->
    Namespace = maps:get(<<"namespace">>, Metadata),
    Name = maps:get(<<"name">>, Metadata),
    ?LOG_INFO(#{what => "HPA should not be watched",
                namespace => Namespace,
                name => Name}),
    State.
