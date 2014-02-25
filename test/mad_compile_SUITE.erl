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

-module(mad_compile_SUITE).

-export([all/0]).
-export([files/1]).
-export([app_src_to_app/1]).
-export([erl_to_beam/1]).
-export([filetype/1]).
-export([deps/1]).
-export([app/1]).
-export([is_compiled/1]).

-import(helper, [get_value/2]).


all() ->
    [
     files, app_src_to_app, erl_to_beam, filetype, deps, app, is_compiled
    ].

files(Config) ->
    DataDir = get_value(data_dir, Config),
    SrcDir = filename:join([DataDir, "deps", "one", "src"]),
    Files = [filename:join(SrcDir, "one.app.src"),
             filename:join(SrcDir, "one.erl"),
             filename:join(SrcDir, "one_src.hrl")],
    Files = mad_compile:files(SrcDir).

app_src_to_app(_) ->
    "/path/to/ebin/file.app" = mad_compile:app_src_to_app("/path/to/ebin",
                                                          "/path/to/file.app.src").

erl_to_beam(_) ->
    "/path/to/ebin/file.beam" = mad_compile:erl_to_beam("/path/to/ebin",
                                                        "/path/to/file.erl").

filetype(_) ->
    ".erl" = mad_compile:filetype("file_.erl"),
    ".erl" = mad_compile:filetype("/path/to/file_.erl"),
    ".erl" = mad_compile:filetype("/path/to/___file_.erl"),
    ".app.src" = mad_compile:filetype("_file.app.src"),
    ".app.src" = mad_compile:filetype("/path/to/file.app.src"),
    ".app.src" = mad_compile:filetype("/path/to/___file_.app.src"),
    ".yrl" = mad_compile:filetype("file_.yrl"),
    ".yrl" = mad_compile:filetype("/path/to/file.yrl"),
    ".yrl" = mad_compile:filetype("/path/to/__file_.yrl"),
    ".erl" = mad_compile:filetype("/path../t.o./file.erl"),
    ".whatever" = mad_compile:filetype("~/.emacs.d/vendor/file.whatever").

deps(Config) ->
    DataDir = get_value(data_dir, Config),
    Deps = [{one, "", {}}, {two, "", {}}],
    ok = mad_compile:deps(DataDir, Config, "rebar.config", Deps),
    pong = one:ping(),
    pong = two:ping(),
    ok = application:load(one),
    ok = application:load(two),
    {ok, [one]} = application:get_key(one, modules),
    {ok, [two]} = application:get_key(two, modules),

    ok = one:test_inc_hrl(),
    ok = one:test_src_hrl(),
    ok = two:test_inc_hrl(),
    ok = two:test_src_hrl().

app(Config) ->
    DataDir = get_value(data_dir, Config),
    ok = mad_compile:app(DataDir, Config, "rebar.config"),
    pong = three:ping(),
    ok = application:load(three),
    {ok, [three]} = application:get_key(three, modules),
    ok = three:test_inc_hrl(),
    ok = three:test_src_hrl().

is_compiled(Config) ->
    DataDir = get_value(data_dir, Config),
    SrcDir = filename:join([DataDir, "deps", "one", "src"]),
    EbinDir = filename:join([SrcDir, "..", "ebin"]),
    BeamFile1 = filename:join(EbinDir, "x_one.beam"),
    BeamFile2 = filename:join(EbinDir, "one.beam"),
    false = mad_compile:is_compiled(BeamFile1, filename:join(SrcDir, "one.erl")),
    true = mad_compile:is_compiled(BeamFile2, filename:join(SrcDir, "one.erl")).
