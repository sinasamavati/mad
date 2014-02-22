%% The MIT License

%% Copyright (c) 2013-2014 Sina Samavati <sina.samv@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(mad_deps_SUITE).

-export([all/0]).
-export([repos_path/1]).
-export([path/1]).
-export([name_and_repo/1]).
-export([checkout_to/1]).
-export([get_publisher/1]).
-export([fetch/1]).

-import(helper, [get_value/2]).


all() ->
    [repos_path, path, name_and_repo, checkout_to, get_publisher, fetch].

repos_path(_) ->
    Path = filename:join([os:cmd("echo -n $HOME"), ".mad", "repos"]),
    Path = mad_deps:repos_path().

path(_) ->
    Path = filename:join([os:cmd("echo -n $HOME"), ".mad", "repos",
                          "publisher", "repo"]),
    Path = mad_deps:path("publisher", "repo").

name_and_repo(_) ->
    Dep = {x,".*",{git,"blah, blah..",{tag,"v0.8.10"}}},
    {"x",{git,"blah, blah..", {tag,"v0.8.10"}}} = mad_deps:name_and_repo(Dep).

checkout_to(_) ->
    "v0.8.10" = mad_deps:checkout_to({tag, "v0.8.10"}),
    "develop" = mad_deps:checkout_to({branch, "develop"}).

get_publisher(_) ->
    "erlang" = mad_deps:get_publisher("git://github.com/erlang/otp.git"),
    "s1n4" = mad_deps:get_publisher("https://github.com/s1n4/mad"),
    "xyz" = mad_deps:get_publisher("https://bitbucket.org/xyz/repo").

fetch(Config) ->
    DataDir = get_value(data_dir, Config),
    DepsDir = filename:join(DataDir, "deps"),
    os:cmd("rm -rf " ++ DepsDir),
    %% make repos and deps directories
    os:cmd("mkdir -p " ++ mad_deps:repos_path()),
    os:cmd("mkdir -p " ++ DepsDir),
    Deps = [{mad, ".*",
             {git, "git://github.com/s1n4/mad.git", {branch, "master"}}
            }],
    mad_deps:fetch(DataDir, Config, "rebar.config", Deps),
    {ok, _} = file:list_dir(filename:join(DepsDir, "mad")),
    os:cmd("rm -rf " ++ DepsDir).
