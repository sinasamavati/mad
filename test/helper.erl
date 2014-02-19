%% This file is part of Mad, and released under the MIT license.
%% See LICENSE for more information.

-module(helper).
-export([get_value/2]).

get_value(Key, Conf) ->
    case lists:keyfind(Key, 1, Conf) of
        {Key, Value} ->
            Value;
        _ ->
            undefined
    end.
