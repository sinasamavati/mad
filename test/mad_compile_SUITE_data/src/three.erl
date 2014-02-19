%% This file is part of Mad, and released under the MIT license.
%% See LICENSE for more information.

-module(three).

-export([test_inc_hrl/0]).
-export([test_src_hrl/0]).
-export([ping/0]).

ping() -> pong.

-include_lib("three_inc.hrl").
-include("three_src.hrl").
