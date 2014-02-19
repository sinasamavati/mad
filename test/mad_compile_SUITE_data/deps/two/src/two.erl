%% This file is part of Mad, and released under the MIT license.
%% See LICENSE for more information.

-module(two).

-export([ping/0]).
-export([test_inc_hrl/0]).
-export([test_src_hrl/0]).

ping() -> pong.

-include_lib("two_inc.hrl").
-include("two_src.hrl").
