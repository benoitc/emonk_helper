%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%%%
%%% This file is part of emonk_helper released under the Apache 2 license. 
%%% See the NOTICE for more information.

-module(emonk_helper_util).

-export([back_off/2]).

back_off(MaxCount, Count) ->
    ScalingFactor = (1 + (MaxCount - Count)) *
        (0.1 + random:uniform(100) * 0.001),
    timer:sleep(erlang:round(500 * ScalingFactor)).


