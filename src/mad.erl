%% This file is part of Mad, and released under the MIT license.
%% See LICENSE for more information.

-module(mad).
-author("Sina Samavati <sina.samv@gmail.com>").

-export([main/1]).
-export(['fetch-deps'/3]).
-export([compile/3]).
-export(['compile-app'/3]).
-export(['compile-deps'/3]).


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
    ConfigFile = get_value(config_file, Opts, "rebar.config"),
    ConfigFileAbs = filename:join(Cwd, ConfigFile),
    Conf = mad_utils:consult(ConfigFileAbs),
    Conf1 = mad_utils:script(ConfigFileAbs, Conf),

    EbinDirs = filename:join([mad_utils:get_value(deps_dir, Conf1, ["deps"]),
                              "*", "ebin"]),
    Paths = ["ebin"|filelib:wildcard(EbinDirs)],
    code:add_paths(Paths),

    %% add lib_dirs to path
    LibDirs = mad_utils:lib_dirs(Cwd, Conf),
    code:add_paths(LibDirs),

    Fun = fun(F) -> ?MODULE:F(Cwd, ConfigFile, Conf1) end,
    lists:foreach(Fun, Params).

%% fetch dependencies
'fetch-deps'(Cwd, ConfigFile, Conf) ->
    case get_value(deps, Conf, []) of
        [] ->
            ok;
        Deps ->
            file:make_dir(mad_deps:repos_path()),
            DepsDir = mad_utils:get_value(deps_dir, Conf, ["deps"]),
            file:make_dir(DepsDir),
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
    Dirs = [mad_utils:sub_dirs(Cwd, ConfigFile, Conf)] ++ [Cwd],
    mad_compile:foreach(fun mad_compile:app/3, Dirs, Conf, ConfigFile).

'compile-deps'(Cwd, ConfigFile, Conf) ->
    mad_compile:deps(Cwd, Conf, ConfigFile, get_value(deps, Conf, [])).

get_value(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {Key, Value} ->
            Value;
        _ -> Default
    end.

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
