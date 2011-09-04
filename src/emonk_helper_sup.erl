%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of emonk_helper released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(emonk_helper_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_pool/2, start_pool/3, stop_pool/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, emonk_helper}, ?MODULE, []).


start_pool(Name, Limit) ->
    start_pool(Name, Limit, fun(_Ctx) -> ok end).

start_pool(Name, Limit, InitFun) ->
    ChildSpec = {Name,
                 {emonk_ctx_pool, start_link, [Name, Limit, InitFun]},
                  permanent, 30000, worker, [emonk_ctx_pool]},
    supervisor:start_child(emonk_helper, ChildSpec).

stop_pool(Name) ->
    supervisor:terminate_child(emonk_helper, Name),
    supervisor:delete_child(emonk_helper, Name).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
        {emonk_ctx_sup,
            {emonk_ctx_sup, start_link, []},
            permanent, infinity, supervisor, [emonk_ctx_sup]}
    ],
    {ok, { {one_for_one, 10, 10}, Children}}.

