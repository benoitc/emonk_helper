%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of emonk_helper released under the Apache 2 license. 
%%% See the NOTICE for more information.
%%% 
%%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.


-module(emonk_ctx_pool).

-export([start_link/3, reload/1, poolsize/1,
         add_ctx/1, mark_idle/1,
         reserve_ctx/1, reserve_batch_ctx/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record('DOWN', {ref, type, pid, info}).
-record(ctx_state, {pid, needs_reload=false}).
-record(state, {name, master, idle, reserve}).

%% --------------------------------------------------
%% public api
%% --------------------------------------------------

start_link(PoolName, ChildCount, InitFun) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName,
            ChildCount, InitFun], []).

reload([]) ->
    ok;
reload(PoolNames) ->
    reload_internal(PoolNames).

poolsize(PoolName) ->
    gen_server:call(PoolName, pool_size, infinity).

add_ctx(PoolName) -> 
    gen_server:cast(PoolName, {add_ctx, self()}).

mark_idle(PoolName) ->
    gen_server:call(PoolName, {mark_idle, self()}, infinity).

reserve_ctx(PoolName) ->
    gen_server:call(PoolName, reserve_ctx, infinity).

reserve_batch_ctx(PoolName, Tries) ->
    reserve_batch_ctx1(PoolName, Tries, Tries).


%% --------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------


init([Name, ChildCount, InitFun]) ->
    Master = ets:new(Name, [private, {keypos, 2}]),
    Idle = ets:new(Name, [private]),
    start_ctxs(Name, InitFun, ChildCount),
    {ok, #state{name=Name, master=Master, idle=Idle}}.

handle_call(reload_ctxs, _From, #state{master=Master, idle=Idle}=State) ->
    reload_idle_ctxs(Idle),
    mark_pending_reloads(Master, Idle),
    {reply, ok, State};

handle_call(reserve_batch_ctx, _From, State) ->
    {Reply, State1} = case handle_call(reserve_ctx, _From, State) of
                          {reply, {ok, Ctx}, NewState} ->
                              emonk_ctx:start_batch(Ctx),
                              {{ok, Ctx}, NewState};
                          {reply, Error, NewState} ->
                              {Error, NewState}
                      end,
    {reply, Reply, State1};

handle_call(reserve_ctx, _From, #state{idle=Idle}=State) ->
    Reply = case ets:first(Idle) of
                '$end_of_table' ->
                    {error, no_ctxs};
                Ctx ->
                    ets:delete(Idle, Ctx),
                    {ok, Ctx}
            end,
    {reply, Reply, State};

handle_call(pool_size, _From, #state{idle=Idle}=State) ->
    {reply, ets:info(Idle, size), State};

handle_call({mark_idle, Ctx}, _From, #state{master=Master,
                                           idle=Idle}=State) ->
    case needs_reload(Master, Ctx) of
        true ->
            emonk_ctx:reload(Ctx),
            clear_reload(Master, Ctx);
        false ->
            ok
    end,
    ets:insert(Idle, {Ctx}),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast({add_ctx, CtxPid}, #state{master=Master, idle=Idle}=State) ->
    erlang:monitor(process, CtxPid),
    CtxState = #ctx_state{pid=CtxPid},
    ets:insert(Master, CtxState),
    ets:insert(Idle, {CtxPid}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'DOWN'{pid=Pid}, #state{master=Master, idle=Idle, name=Name}=State) ->
    ets:delete(Master, Pid),
    ets:delete(Idle, Pid),
    emonk_ctx_sup:start_ctx(self(), Name),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------
%% private functions 
%% --------------------------------------------------

reload_internal([Name|Rest]) when is_atom(Name) ->
    gen_server:call(Name, reload_ctxs, infinity),
    reload_internal(Rest);
reload_internal([Name|Rest]) when is_list(Name) ->
    reload_internal([list_to_existing_atom(Name)|Rest]);
reload_internal([]) ->
    ok.

needs_reload(Master, CtxPid) ->
    [CtxState] = ets:lookup(Master, CtxPid),
    CtxState#ctx_state.needs_reload.

clear_reload(Master, CtxPid) ->
    [CtxState] = ets:lookup(Master, CtxPid),
    CtxState1 = CtxState#ctx_state{needs_reload=false},
    ets:insert(Master, CtxState1).

is_ctx_idle(Idle, CtxPid) ->
    case ets:lookup(Idle, {CtxPid}) of
        [] ->
            false;
        _ ->
            true
    end.

start_ctxs(_PoolName, _InitFun, 0) ->
    ok;
start_ctxs(PoolName, InitFun, Count) ->
    emonk_ctx_sup:start_ctx(self(), PoolName, InitFun),
    start_ctxs(PoolName, InitFun, Count - 1).

reload_idle_ctxs(Tid) ->
    reload_idle_ctxs(ets:first(Tid), Tid).

reload_idle_ctxs('$end_of_table', _Tid) ->
    ok;
reload_idle_ctxs(Current, Tid) ->
    emonk_ctx:reload(Current),
    reload_idle_ctxs(ets:next(Tid, Current), Tid).

mark_pending_reloads(Master, Idle) ->
    mark_pending_reloads(ets:first(Master), Master, Idle).

mark_pending_reloads('$end_of_table', _Master, _Idle) ->
    ok;
mark_pending_reloads(CtxPid, Master, Idle) ->
    case is_ctx_idle(Idle, CtxPid) of
        true ->
            ok;
        false ->
            [CtxState] = ets:lookup(Master, CtxPid),
            CtxState1 = CtxState#ctx_state{needs_reload=true},
            ets:insert(Master, CtxState1)
    end,
    mark_pending_reloads(ets:next(Master, CtxPid), Master, Idle).

reserve_batch_ctx1(_Name, _MaxCount, 0) ->
    {error, no_ctxs};
reserve_batch_ctx1(Name, MaxCount, Count) ->
    case gen_server:call(Name, reserve_batch_ctx) of
        {error, no_ctxs} ->
            emonk_helper_util:back_off(MaxCount, Count),
            reserve_batch_ctx1(Name, MaxCount, Count - 1);
        {ok, Ctx} ->
            {ok, Ctx}
    end.
