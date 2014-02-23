-module(mad_deps_tests).

-include("test.hrl").


repos_path_test() ->
    Path = filename:join([os:cmd("echo -n $HOME"), ".mad", "repos"]),
    Path = mad_deps:repos_path().

path_test() ->
    Path = filename:join([os:cmd("echo -n $HOME"), ".mad", "repos",
                          "publisher", "repo"]),
    Path = mad_deps:path("publisher", "repo").

name_and_repo_test() ->
    Dep = {x,".*",{git,"blah, blah..",{tag,"v0.8.10"}}},
    {"x",{git,"blah, blah..", {tag,"v0.8.10"}}} = mad_deps:name_and_repo(Dep).

checkout_to_test() ->
    "v0.8.10" = mad_deps:checkout_to({tag, "v0.8.10"}),
    "develop" = mad_deps:checkout_to({branch, "develop"}).

get_publisher_test() ->
    "erlang" = mad_deps:get_publisher("git://github.com/erlang/otp.git"),
    "s1n4" = mad_deps:get_publisher("https://github.com/s1n4/mad"),
    "xyz" = mad_deps:get_publisher("https://bitbucket.org/xyz/repo").

fetch_test() ->
    DepsDir = filename:join(?DATA_DIR, "deps"),
    os:cmd("rm -rf " ++ DepsDir),
    %% make repos and deps directories
    os:cmd("mkdir -p " ++ mad_deps:repos_path()),
    os:cmd("mkdir -p " ++ DepsDir),
    Deps = [{mad, ".*",
             {git, "git://github.com/s1n4/mad.git", {branch, "master"}}
            }],
    mad_deps:fetch(?DATA_DIR, [], "rebar.config", Deps),
    {ok, _} = file:list_dir(filename:join(DepsDir, "mad")),
    os:cmd("rm -rf " ++ DepsDir).
