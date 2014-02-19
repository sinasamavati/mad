%% This file is part of Mad, and released under the MIT license.
%% See LICENSE for more information.

-module(one).

-export([ping/0]).
-export([test_inc_hrl/0]).
-export([test_src_hrl/0]).

-include("one_src.hrl").
-include_lib("one_inc.hrl").

ping() -> pong.

test_inc_hrl() -> ?INC_HRL.

test_src_hrl() -> ?SRC_HRL.
