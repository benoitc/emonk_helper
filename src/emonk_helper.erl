%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of emonk_helper released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(emonk_helper).

-export([start_pool/2, start_pool/3, stop_pool/1, poolsize/1,
         reserve_ctx/1, reserve_batch_ctx/2,
         call/4, call_async/4,
         eval/3, eval_async/3]).

%% --------------------------------------------------
%% public api
%% --------------------------------------------------

start_pool(Name, Limit) ->
    emonk_helper_sup:start_pool(Name, Limit).

start_pool(Name, Limit, InitFun) ->
    emonk_helper_sup:start_pool(Name, Limit, InitFun).

stop_pool(Name) ->
    emonk_helper_sup:stop(Name).

poolsize(Name) ->
    monk_ctx_pool:poolsize(Name).

reserve_ctx(PoolName) ->
    emonk_ctx_pool:reserve_ctx(PoolName).

reserve_batch_ctx(PoolName, Tries) ->
    emonk_ctx_pool:reserve_batch_ctx(PoolName, Tries).

call(CtxPid, Name, Args, Tries) ->
    call1(CtxPid, Name, Args, Tries, Tries).

call_async(CtxPid, Name, Args, Tries) ->
    call_async1(CtxPid, Name, Args, Tries, Tries).

eval(CtxPid, Args, Tries) ->
    eval1(CtxPid, Args, Tries, Tries).

eval_async(CtxPid, Args, Tries) ->
    eval_async1(CtxPid, Args, Tries, Tries).


%% --------------------------------------------------
%% private functions 
%% --------------------------------------------------

call1(_PoolName, _Name, _Args, _MaxCount, 0) ->
    {error, no_ctxs};
call1(PoolName, Name, Args, MaxCount, Count) ->
    case reserve_ctx(PoolName) of
        {ok, CtxPid} ->
            emonk_ctx:call(CtxPid, Name, Args);
        {error, no_ctxs} ->
            emonk_helper_util:back_off(MaxCount, Count),
            call1(PoolName, Name, Args, MaxCount, Count - 1)
    end.

call_async1(_PoolName, _Name, _Args, _MaxCount, 0) ->
    {error, no_ctxs};
call_async1(PoolName, Name, Args, MaxCount, Count) ->
    case reserve_ctx(PoolName) of
        {ok, CtxPid} ->
            JobId = {CtxPid, make_ref()},
            emonk_ctx:call_async(CtxPid, self(), JobId, Name, Args),
            {ok, JobId};
        {error, no_ctxs} ->
            emonk_helper_util:back_off(MaxCount, Count),
            call_async1(PoolName, Name, Args, MaxCount, Count - 1)
    end.

eval1(_PoolName, _Script, _MaxCount, 0) ->
    {error, no_ctxs};
eval1(PoolName, Script, MaxCount, Count) ->
    case reserve_ctx(PoolName) of
        {ok, CtxPid} ->
            emonk_ctx:eval(CtxPid, Script);
        {error, no_ctxs} ->
            emonk_helper_util:back_off(MaxCount, Count),
            eval1(PoolName, Script, MaxCount, Count - 1)
    end.

eval_async1(_PoolName, _Script, _MaxCount, 0) ->
    {error, no_ctxs};
eval_async1(PoolName, Script, MaxCount, Count) ->
    case reserve_ctx(PoolName) of
        {ok, CtxPid} ->
            JobId = {CtxPid, make_ref()},
            emonk_ctx:eval_async(CtxPid, self(), JobId, Script),
            {ok, JobId};
        {error, no_ctxs} ->
            emonk_helper_util:back_off(MaxCount, Count),
            eval_async1(PoolName, Script, MaxCount, Count - 1)
    end.
