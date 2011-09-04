%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of emonk_helper released under the Apache 2 license. 
%%% See the NOTICE for more information.

%% dispatch work to emonk context


-module(emonk_ctx).

-export([start_link/3, reload/1, start_batch/1, finish_batch/1, 
         call/3, call/4, call_async/5,
         eval/2, eval/3, eval_async/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).


-record(state, {
        pool,
        pool_name,
        init_fun,
        ctx,
        in_batch=false}).

%% --------------------------------------------------
%% public api
%% --------------------------------------------------

start_link(Pool, PoolName, InitFun) ->
    gen_server:start_link(?MODULE, [Pool, PoolName, InitFun], []).

reload(CtxPid) ->
    gen_server:cast(CtxPid, reload).

start_batch(CtxPid) ->
    gen_server:call(CtxPid, start_batch, infinity).

finish_batch(CtxPid) ->
    gen_server:call(CtxPid, finish_batch, infinity).

call(CtxPid, Name, Args) ->
    call(CtxPid, Name, Args, infinity).

call(CtxPid, Name, Args, Timeout) ->
    gen_server:call(CtxPid, {call, Name, Args}, Timeout).

call_async(CtxPid, Sender, JobId, Name, Args) ->
    gen_server:cast(CtxPid, {call, Sender, JobId, Name, Args}).

eval(CtxPid, Script) ->
    eval(CtxPid, Script, infinity).

eval(CtxPid, Script, Timeout) ->
    gen_server:call(CtxPid, {eval, Script}, Timeout).

eval_async(CtxPid, Sender, JobId, Script) ->
    gen_server:cast(CtxPid, {eval, Sender, JobId, Script}).

%% --------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------


init([Pool, PoolName, InitFun]) ->
    {ok, Ctx} = new_context(InitFun),

    %% add this context to the pool
    emonk_ctx_pool:add_ctx(PoolName),

    %% monitor the pool
    erlang:monitor(process, Pool),
    {ok, #state{pool = Pool,
                pool_name = PoolName,
                init_fun = InitFun,
                ctx = Ctx}}.

handle_call(start_batch, _From, State) ->
    {reply, ok, State#state{in_batch=true}};

handle_call(finish_batch, _From, State) ->
    NewState = State#state{in_batch=false},
    maybe_idle(NewState),
    {reply, ok, NewState};

handle_call({call, Name, Args}, _From, #state{ctx=Ctx}=State) ->
    Reply = emonk:call(Ctx, Name, Args),
    maybe_idle(State),
    {reply, Reply, State};

handle_call({eval, Script}, _From, #state{ctx=Ctx}=State) ->
    Reply = emonk:eval(Ctx, Script),
    maybe_idle(State),
    {reply, Reply, State};
    
handle_call(_Msg, _From, State) ->
    {reply, ignore, State}.

handle_cast(reload, State) ->
    #state{ ctx = Ctx,
            init_fun = InitFun } = State,
    init_context(Ctx, InitFun),
    {noreply, State};

handle_cast({call, Sender, JobId, Name, Args}, #state{ctx=Ctx}=State) ->
    Reply = emonk:call(Ctx, Name, Args),
    Sender ! {JobId, {reply, Reply}},
    maybe_idle(State),
    {noreply, State};

handle_cast({eval, Sender, JobId, Script}, #state{ctx=Ctx}=State) ->
    Reply = emonk:eval(Ctx, Script),
    Sender ! {JobId, {reply, Reply}},
    maybe_idle(State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MRef, _Type, Pool, _Info}, #state{pool=Pool}=State) ->
    {stop, normal, State#state{pool=undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------
%% private functions 
%% --------------------------------------------------

init_context(Ctx, InitFun) ->
    case InitFun(Ctx) of
    ok -> {ok, Ctx};
    {ok, NewCtx} -> {ok, NewCtx};
    Error -> Error
    end.

new_context(InitFun) ->
    {ok, Ctx} = emonk:create_ctx(),
    init_context(Ctx, InitFun).

maybe_idle(#state{in_batch=false, pool_name=PoolName}) ->
    emonk_ctx_pool:mark_idle(PoolName);
maybe_idle(#state{in_batch=true}) ->
    ok.
