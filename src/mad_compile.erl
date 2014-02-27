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

-module(mad_compile).

-export([deps/4]).
-export([app/3]).
-export([foreach/4]).

-define(COMPILE_OPTS(Inc, Ebin, Opts),
        [report, {i, Inc}, {outdir, Ebin}] ++ Opts).

-type directory() :: string().
-type filename() :: string().

-include("mad.hrl").

%% compile dependencies
-spec deps(directory(), any(), filename(), [mad_deps:dependency()]) -> ok.
deps(_, _, _, []) ->
    ok;
deps(Cwd, Conf, ConfigFile, [H|T]) ->
    {Name, _} = mad_deps:name_and_repo(H),
    case get(Name) of
        compiled ->
            ok;
        _ ->
            dep(Cwd, Conf, ConfigFile, Name)
    end,
    deps(Cwd, Conf, ConfigFile, T).

%% compile a dependency
-spec dep(directory(), any(), filename(), string()) -> ok.
dep(Cwd, Conf, ConfigFile, Name) ->
    %% check dependencies of the dependency
    DepPath = filename:join([Cwd, ?deps_dir(Conf), Name]),
    DepConfigFile = filename:join(DepPath, ConfigFile),

    %% read rebar config file and evaluate rebar script file
    DepConf = mad_utils:consult(DepConfigFile),
    DepConf1 = mad_utils:script(DepConfigFile, DepConf),

    deps(Cwd, Conf, ConfigFile, ?deps(DepConf1)),

    %% add lib_dirs to path
    LibDirs = mad_utils:lib_dirs(DepPath, DepConf1),
    code:add_paths(LibDirs),

    %% compile sub_dirs and add them to path
    SubDirs = mad_utils:sub_dirs(DepPath, DepConfigFile, DepConf1),
    foreach(fun app/3, SubDirs, DepConf1, DepConfigFile),

    app(DepPath, DepConf, DepConfigFile),
    put(Name, compiled),
    ok.

-spec app(directory(), any(), filename()) -> ok.
app(Dir, Conf, ConfigFile) ->
    Conf1 = mad_utils:script(ConfigFile, Conf),
    SrcDir = mad_utils:src(Dir),

    case files(SrcDir) of
        [] ->
            ok;
        Files ->
            IncDir = mad_utils:include(Dir),
            EbinDir = mad_utils:ebin(Dir),

            %% create EbinDir and add it to code path
            file:make_dir(EbinDir),
            code:add_path(EbinDir),

            Opts = mad_utils:get_value(erl_opts, Conf1, []),
            lists:foreach(compile_fun(IncDir, EbinDir, Opts), sort_files(Files))
    end,
    dtl(Dir, Conf1),
    ok.

%% ask for erlydtl_opts and compile erlydtl templates
dtl(Dir, Config) ->
    case mad_utils:get_value(erlydtl_opts, Config, []) of
        [] -> skip;
        X ->
            io:format("Dir: ~p DTL: ~p~n", [Dir, X]),
            compile_erlydtl_files(validate_erlydtl_opts(Dir, X))
    end.

-spec validate_property({atom(), term()}, term()) -> {atom(), term()}.
validate_property({modules, _}, Modules) ->
    {modules, Modules};
validate_property(Else, _) ->
    Else.

-spec compile_fun(directory(), directory(), [compile:option()]) ->
                         fun((file:name()) -> ok).
compile_fun(Inc, Bin, Opt) ->
    fun(File) ->
            compile(File, Inc, Bin, Opt, filetype(File))
    end.

filetype(File) ->
    Name = filename:basename(File),
    L = length(hd(string:tokens(Name, "."))),
    string:substr(Name, L + 1, length(Name)).

compile(File, Inc, Bin, Opts, ".xrl") ->
    compile_xyrl(File, Inc, Bin, Opts, ".erl", leex);
compile(File, Inc, Bin, Opts, ".yrl") ->
    compile_xyrl(File, Inc, Bin, Opts, ".erl", yecc);
compile(File, Inc, Bin, Opts, ".erl") ->
    BeamFile = erl_to_beam(Bin, File),
    Compiled = is_compiled(BeamFile, File),
    if  Compiled =:= false ->
            io:format("Compiling ~s~n", [File]),
            Opts1 = ?COMPILE_OPTS(Inc, Bin, Opts),
            compile:file(File, Opts1),
            ok;
        true -> ok
    end;
compile(File, _Inc, Bin, _Opts, ".app.src") ->
    AppFile = app_src_to_app(Bin, File),
    io:format("Writing ~s~n", [AppFile]),
    BeamFiles = filelib:wildcard("*.beam", Bin),
    Modules = [list_to_atom(filename:basename(X, ".beam")) || X <- BeamFiles],
    [Struct|_] = mad_utils:consult(File),
    {application, AppName, Props} = Struct,
    Props1 = add_modules_property(Props),
    Props2 = [validate_property(X, Modules) || X <- Props1],
    Struct1 = {application, AppName, Props2},
    file:write_file(AppFile, io_lib:format("~p.~n", [Struct1])),
    ok;
compile(_, _, _, _, _) ->
    unknown_files.

compile_xyrl(File, Inc, Bin, Opts, Type, Mod) ->
    Target = to_erl(File),
    case is_compiled(Target, File) of
        false ->
            Mod:file(File, [{verbose, true}]),
            compile(Target, Inc, Bin, Opts, Type);
        true ->
            ok
    end.

-spec files(directory()) -> [filename()].
files(Dir) ->
    filelib:fold_files(Dir, ".*", true, fun(F, Acc) -> [F|Acc] end, []).

-spec app_src_to_app(directory(), filename()) -> filename().
app_src_to_app(Bin, Filename) ->
    filename:join(Bin, filename:basename(Filename, ".app.src") ++ ".app").

-spec erl_to_beam(directory(), filename()) -> filename().
erl_to_beam(Bin, Filename) ->
    filename:join(Bin, filename:basename(Filename, ".erl") ++ ".beam").

-spec to_erl(filename()) -> filename().
to_erl(Filename) ->
    filename:rootname(Filename) ++ ".erl".

-spec is_compiled(directory(), file:name()) -> boolean().
is_compiled(Target, File) ->
    mad_utils:last_modified(Target) >= mad_utils:last_modified(File).

-spec add_modules_property([{atom(), term()}]) -> [{atom(), term()}].
add_modules_property(Properties) ->
    case mad_utils:get_value(modules, Properties, undefined) of
        undefined ->
            Properties ++ [{modules, []}];
        _ ->
            Properties
    end.

-spec split_files([filename()]) -> {[filename()], [filename()], [filename()]}.
split_files(Files) ->
    split_files(Files, [], [], []).

-spec split_files([filename()], [filename()], [filename()], [filename()]) ->
                         {[filename()], [filename()], [filename()]}.
split_files([], XYrlFiles, ErlFiles, OtherFiles) ->
    {XYrlFiles, ErlFiles, OtherFiles};
split_files([File|Rest], XYrlFiles, ErlFiles, OtherFiles) ->
    {A, B, C} = case filetype(File) of
                    ".xrl" ->
                        {[File|XYrlFiles], ErlFiles, OtherFiles};
                    ".yrl" ->
                        {XYrlFiles ++ [File], ErlFiles, OtherFiles};
                    ".erl" ->
                        {XYrlFiles, ErlFiles ++ [File], OtherFiles};
                    _ ->
                        {XYrlFiles, ErlFiles, OtherFiles ++ [File]}
                end,
    split_files(Rest, A, B, C).

-spec sort_files([filename()]) -> [filename()].
sort_files(Files) ->
    {XYrlFiles, ErlFiles, OtherFiles} = split_files(Files),
    ErlFiles1 = sort_files_by_priority(ErlFiles, [], [], []),
    XYrlFiles ++ ErlFiles1 ++ OtherFiles.

-spec sort_files_by_priority([filename()], [filename()], [filename()],
                             [filename()]) -> [filename()].
sort_files_by_priority([], High, Medium, Low) ->
    (High ++ Medium) ++ Low;
sort_files_by_priority([File|Rest], High, Medium, Low) ->
    {High1, Medium1, Low1} =
        case is_behaviour(File) of
            true ->
                {[File|High], Medium, Low};
            false ->
                {High, [File|Medium], Low}
        end,
    {High2, Medium2, Low2} =
        case uses_parse_transform(File) of
            false ->
                {High1, Medium1, Low1};
            true ->
                {High1 -- [File], Medium1 -- [File], [File|Low1]}
        end,
    sort_files_by_priority(Rest, High2, Medium2, Low2).

-spec foreach(fun((directory(), filename()) -> ok), [filename()], any(),
              filename()) -> ok.
foreach(_, [], _, _) ->
    ok;
foreach(Fun, [Dir|T], Config, ConfigFile) ->
    Fun(Dir, Config, ConfigFile),
    foreach(Fun, T, Config, ConfigFile).

-spec uses_parse_transform(file:name()) -> boolean().
uses_parse_transform(File) ->
    [] =/= mad_utils:exec("grep", ["-sE", "\"\\{[ \\r\\n\\t]*parse_transform\"",
                                   File]).

-spec is_behaviour(file:name()) -> boolean().
is_behaviour(File) ->
    [] =/= mad_utils:exec("grep", ["-sE", "\"\\-callback|behaviour_info\\/1\"",
                                   File]).

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

validate_erlydtl_opts(Cwd, Opts) ->
    DefaultDocRoot = filename:join("priv", "templates"),
    {DocRoot, Opts1} = get_kv(doc_root, Opts, DefaultDocRoot),
    {OutDir, Opts2} = get_kv(out_dir, Opts1, "ebin"),
    {CompilerOpts, Opts3} = get_kv(compiler_options, Opts2, []),
    {SourceExt, Opts4} = get_kv(source_ext, Opts3, ".dtl"),
    {ModuleExt, Opts5} = get_kv(module_ext, Opts4, ""),

    {_, DocRootDir} = DocRoot,
    DocRoot1 = {doc_root, filename:join(Cwd, DocRootDir)},
    {_, OutDir1} = OutDir,
    OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},

    [DocRoot1, OutDir2, CompilerOpts, SourceExt, ModuleExt|Opts5].

module_name(File, Ext, NewExt) ->
    list_to_atom(filename:basename(File, Ext) ++ NewExt).

compile_erlydtl_files(Opts) ->
    {{_, DocRoot}, Opts1} = get_kv(doc_root, Opts, ""),
    {{_, SourceExt}, Opts2} = get_kv(source_ext, Opts1, ""),
    {{_, ModuleExt}, Opts3} = get_kv(module_ext, Opts2, ""),
    {{_, OutDir}, _} = get_kv(out_dir, Opts3, ""),

    Files = filelib:fold_files(DocRoot, SourceExt, true,
                               fun(F, Acc) -> [F|Acc] end, []),

    Compile = fun(F) ->
                      ModuleName = module_name(F, SourceExt, ModuleExt),
                      BeamFile = erl_to_beam(OutDir, atom_to_list(ModuleName)),
                      Compiled = is_compiled(BeamFile, F),
                      if Compiled =:= false ->
                              io:format("DTL Compiling ~s~n", [F]),
                              erlydtl:compile(F, ModuleName, Opts3);
                         true -> ok
                      end
              end,

    lists:foreach(Compile, Files).
