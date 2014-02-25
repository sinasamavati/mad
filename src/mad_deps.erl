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

-module(mad_deps).

-export([repos_path/0]).
-export([path/2]).
-export([fetch/4]).
-export([name_and_repo/1]).
-export([checkout_to/1]).
-export([get_publisher/1]).

-define(REPOS_PATH, filename:join([mad_utils:home(), ".mad", "repos"])).

-type directory() :: string().
-type filename() :: string().
-type name() :: atom().
-type uri() :: string().
-type version_control() :: git | hg.
-type repo() :: {version_control(), uri(), {branch | tag, string()} | string()}.
-type dependency() :: {name(), string(), repo()}.
-export_type([dependency/0]).


-spec repos_path() -> directory().
repos_path() ->
    %% ~/.mad/repos
    ?REPOS_PATH.

-spec path(string(), string()) -> directory().
path(Publisher, Repo) ->
    %% ~/.mad/repos/Publisher/Repo
    filename:join([?REPOS_PATH, Publisher, Repo]).

-spec fetch(directory(), any(), filename(), [dependency()]) -> ok.
fetch(_, _Config, _, []) ->
    ok;
fetch(Cwd, Config, ConfigFile, [H|T]) when is_tuple(H) =:= false ->
    fetch(Cwd, Config, ConfigFile, T);
fetch(Cwd, Config, ConfigFile, [H|T]) ->
    {Name, Repo} = name_and_repo(H),
    {Cmd, Uri, Co} = case Repo of
                         V={_, _, _} ->
                             V;
                         {_Cmd, _Url, _Co, _} ->
                             {_Cmd, _Url, _Co}
                     end,
    Cmd1 = atom_to_list(Cmd),
    Co1 = checkout_to(Co),
    Publisher = get_publisher(Uri),
    case get(Name) of
        fetched ->
            ok;
        _ ->
            fetch_dep(Cwd, Config, ConfigFile, Publisher, Name, Cmd1, Uri),
            build_dep(Cwd, Config, ConfigFile, Publisher, Name, Cmd1, Co1)
    end,
    fetch(Cwd, Config, ConfigFile, T).

-spec fetch_dep(directory(), any(), filename(), string(), string(), string(), uri())
               -> ok.
fetch_dep(Cwd, Config, ConfigFile, Publisher, Name, Cmd, Uri) ->
    TrunkPath = path(Publisher, Name),
    Opts = ["clone", Uri, TrunkPath],
    io:format("dependency: ~s~n", [Name]),
    %% fetch
    mad_utils:exec(Cmd, Opts),
    put(Name, fetched),

    %% check dependencies of the dependency
    TrunkConfigFile = filename:join(TrunkPath, ConfigFile),
    DepConf = mad_utils:consult(TrunkConfigFile),
    DepConf1 = mad_utils:script(TrunkConfigFile, DepConf),
    fetch(Cwd, Config, ConfigFile, mad_utils:get_value(deps, DepConf1, [])).

%% build dependency based on branch/tag/commit
-spec build_dep(directory(), any(), string(), string(), string(), string(),
                string()) -> ok.
build_dep(Cwd, Conf, _ConfigFile, Publisher, Name, Cmd, Co) ->
    TrunkPath = path(Publisher, Name),
    DepsDir = filename:join(Cwd, mad_utils:get_value(deps_dir, Conf, "deps")),
    %% get a copy of dependency from trunk
    mad_utils:exec("cp", ["-r", TrunkPath, DepsDir]),
    DepPath = filename:join(DepsDir, Name),
    %% change cwd to the copy of trunk and checkout to Co
    ok = file:set_cwd(DepPath),
    mad_utils:exec(Cmd, ["checkout", Co]),
    ok = file:set_cwd(Cwd).


%% internal
-spec name_and_repo(dependency()) -> {string(), repo()}.
name_and_repo({Name, _, Repo}) ->
    {atom_to_list(Name), Repo};
name_and_repo({Name, _, Repo, _}) ->
    {atom_to_list(Name), Repo}.

-spec checkout_to(term() | {any(), string}) -> term().
checkout_to({_, V}) -> V;
checkout_to(Else) -> Else.

-spec get_publisher(uri()) -> string().
get_publisher(Uri) ->
    S = [{git, 9418}|uri_defaults:scheme_defaults()],
    {ok, {_, _, _, _, Path, _}} = uri:parse(Uri, [{scheme_defaults, S}]),
    [Publisher|_] = string:tokens(Path, "/"),
    Publisher.
