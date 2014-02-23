-module(mad_utils_tests).

-include("test.hrl").


cwd_test() ->
    Cwd = os:cmd("pwd") -- "\n",
    Cwd = mad_utils:cwd().

exec_test() ->
    "xyz" = mad_utils:exec("echo", ["-n", "xyz"]).

home_test() ->
    Home = os:cmd("echo $HOME") -- "\n",
    Home = mad_utils:home().

consult_test() ->
    File = filename:join(?DATA_DIR, "rebar"),
    [] = mad_utils:consult(File),
    [{deps, [
             {mad, ".*", {git, "git://github.com/s1n4/mad.git",
                          {branch, "master"}}}
            ]},
     {erl_opts, [d, 'X']}] = mad_utils:consult(File ++ ".config").

src_test() ->
    "/path/to/app/src" = mad_utils:src("/path/to/app").

include_test() ->
    "/path/to/app/include" = mad_utils:include("/path/to/app").

ebin_test() ->
    "/path/to/app/ebin" = mad_utils:ebin("/path/to/app").

deps_test() ->
    File = filename:join(?DATA_DIR, "rebar"),
    [] = mad_utils:deps(File),
    [{mad, ".*",
      {git, "git://github.com/s1n4/mad.git", {branch, "master"}
      }}] = mad_utils:deps(File ++ ".config").

get_value_test() ->
    Opts = [{numbers, [0,1,2,"and so on"]}],
    patience_dude = mad_utils:get_value(gimme_wat_I_want, Opts, patience_dude),
    [0,1,2,"and so on"] = mad_utils:get_value(numbers, Opts, undefined).

script_test() ->
    [a, b, c] = mad_utils:script("rebar.config", [a, b, c]),
    File = filename:join(?DATA_DIR, "rebar.config"),
    [{sub_dirs, ["sub_dir1", "sub_dir2"]},
     a, b, c] = mad_utils:script(File, [a, b, c]).

sub_dirs_test() ->
    ["/sub_dir0"] = mad_utils:sub_dirs("/", "rebar.config",
                                       [{sub_dirs, ["sub_dir0"]}]),
    DataDir = ?DATA_DIR,
    SD1 = filename:absname(filename:join(DataDir, "sub_dir1")),
    SD2 = filename:join(SD1, "trap"),
    SD3 = filename:absname(filename:join(DataDir, "sub_dir2")),
    SD4 = filename:join(SD3, "time-machine"),
    [
     SD1, SD2, SD3, SD4
    ] =  mad_utils:sub_dirs(DataDir, "rebar.config",
                            [{sub_dirs, ["sub_dir1", "sub_dir2"]}]).

lib_dirs_test() ->
    [] = mad_utils:lib_dirs("/", [{lib_dirs, ["lib_dir0"]}]),
    DataDir = ?DATA_DIR,
    LD1 = filename:absname(filename:join([DataDir, "lib_dir1", "app1", "ebin"])),
    LD2 = filename:absname(filename:join([DataDir, "lib_dir2", "app2", "ebin"])),
    [LD1, LD2] = mad_utils:lib_dirs(DataDir,
                                    [{lib_dirs, ["lib_dir1", "lib_dir2"]}]).

https_to_git_test() ->
    Repo = "https://github.com/erlang/otp.git",
    "git://github.com/erlang/otp.git" = mad_utils:https_to_git(Repo).

git_to_https_test() ->
    Repo = "git://github.com/s1n4/some_secret.git",
    "https://github.com/s1n4/some_secret.git" = mad_utils:git_to_https(Repo).

last_modified_test() ->
    0 = mad_utils:last_modified("you_mad_bro"),
    File = filename:join(?DATA_DIR, "rebar.config"),
    true = (mad_utils:last_modified(File) > 0).
