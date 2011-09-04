%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of emonk_helper released under the Apache 2 license. 
%%% See the NOTICE for more information.

%% @doc supervise emonk contexts

-module(emonk_ctx_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, stop/1]).
-export([start_ctx/3]).

start_ctx(Pool, PoolName, InitFun) when is_pid(Pool) ->
    supervisor:start_child(?MODULE, [Pool, PoolName, InitFun]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_S) -> ok.

%% @private
init([]) ->
    {ok,
     {{simple_one_for_one, 10, 10},
      [{undefined,
        {emonk_ctx, start_link, []},
        temporary, 3000, worker, [emonk_ctx]}]}}.
