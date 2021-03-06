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

-module(mad).

-export([main/1]).
-export(['fetch-deps'/3]).
-export([compile/3]).
-export(['compile-app'/3]).
-export(['compile-deps'/3]).

-include("mad.hrl").

main([]) ->
    help();
main(Args) ->
    {Opts, Params} = case getopt:parse(option_spec_list(), Args) of
                         {ok, {Opts1, Params1}} ->
                             {Opts1, [list_to_atom(E) || E <- Params1]};
                         {error, {Reason, Data}} ->
                             help(Reason, Data)
                     end,
    maybe_invalid(Params),
    maybe_help(Opts, Params),
    maybe_version_info(Opts),

    Cwd = mad_utils:cwd(),
    ConfigFile = mad_utils:get_value(config_file, Opts, "rebar.config"),
    ConfigFileAbs = filename:join(Cwd, ConfigFile),
    Conf = mad_utils:consult(ConfigFileAbs),
    Conf1 = mad_utils:script(ConfigFileAbs, Conf),

    %% -pa ebin deps/*/ebin
    EbinDirs = filename:join([?deps_dir(Conf), "*", "ebin"]),
    Paths = ["ebin"|filelib:wildcard(EbinDirs)],
    code:add_paths(Paths),

    %% add lib_dirs to path
    LibDirs = mad_utils:lib_dirs(Cwd, Conf),
    code:add_paths(LibDirs),

    Fun = fun(F) -> ?MODULE:F(Cwd, ConfigFile, Conf1) end,
    lists:foreach(Fun, Params).

%% fetch dependencies
'fetch-deps'(Cwd, ConfigFile, Conf) ->
    case ?deps(Conf) of
        [] ->
            ok;
        Deps ->
            file:make_dir(mad_deps:repos_path()),
            file:make_dir(?deps_dir(Conf)),
            mad_deps:fetch(Cwd, Conf, ConfigFile, Deps)
    end.

%% compile dependencies and the app
compile(Cwd, ConfigFile, Conf) ->
    %% compile dependencies
    'compile-deps'(Cwd, ConfigFile, Conf),

    %% compile the app
    'compile-app'(Cwd, ConfigFile, Conf).

%% compile a project according to the conventions
'compile-app'(Cwd, ConfigFile, Conf) ->
    %% check sub_dirs if they have something to be compiled
    SubDirs = mad_utils:sub_dirs(Cwd, ConfigFile, Conf),
    Dirs = SubDirs ++ [Cwd],
    mad_compile:foreach(fun mad_compile:app/3, Dirs, Conf, ConfigFile).

'compile-deps'(Cwd, ConfigFile, Conf) ->
    mad_compile:deps(Cwd, Conf, ConfigFile, ?deps(Conf)).

option_spec_list_() ->
    [
     {help, $h, "help", undefined, "Displays this message"},
     {config_file, $C, "config", {string, "rebar.config"}, "Rebar config file to use"}
    ].

maybe_help(Opts, Params) ->
    Fun = fun(L) ->
                  case lists:member(help, L) of
                      true ->
                          help(),
                          halt(0);
                      false ->
                          ok
                  end
          end,
    Fun(Opts),
    Fun(Params).

maybe_invalid(Params) ->
    lists:foreach(fun(E) ->
                          case erlang:function_exported(?MODULE, E, 3) of
                              true -> ok;
                              false -> help("invalid_parameter", E)
                          end
                  end, Params).

help("invalid_parameter", Data) ->
    help(io_lib:format("invalid_parameter \"~s\"", [Data]));
help(Reason, Data) ->
    help(io_lib:format("~s ~p", [Reason, Data])).

help(Msg) ->
    io:format("Error: ~s~n~n", [Msg]),
    help(),
    halt(1).

help() ->
    Params = [
              {"", ""},
              {"fetch-deps", "Fetches dependencies"},
              {"compile-deps", "Compiles dependencies"},
              {"compile-app", "Compiles application"},
              {"compile", "Compiles dependencies and application"}
             ],
    getopt:usage(option_spec_list(), escript:script_name(), "", Params).

-ifdef(GIT_REVISION).
option_spec_list() ->
    option_spec_list_() ++ [{version, undefined, "version", undefined,
                             "Displays version information"}].

maybe_version_info(Opts) ->
    case lists:member(version, Opts) of
        true ->
            version_info();
        false ->
            ok
    end.

version_info() ->
    io:format("mad ~s (~s) ~s ~p~n", [?VSN_NUMBER, ?GIT_REVISION,
                                      ?OTP_RELEASE, ?TIMESTAMP]),
    halt().

-else.
option_spec_list() ->
    option_spec_list_().

maybe_version_info(_) -> ok.

-endif.
