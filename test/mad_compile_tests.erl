-module(mad_compile_tests).

-include("test.hrl").


erl_files_test() ->
    SrcDir = filename:join([?DATA_DIR, "deps", "one", "src"]),
    ErlFile = filename:join(SrcDir, "one.erl"),
    [ErlFile] = mad_compile:erl_files(SrcDir).

app_src_files_test() ->
    SrcDir = filename:join([?DATA_DIR, "deps", "one", "src"]),
    AppSrcFile = filename:join(SrcDir, "one.app.src"),
    [AppSrcFile] = mad_compile:app_src_files(SrcDir).

is_app_src_test() ->
    false = mad_compile:is_app_src("/path/to/file.erl"),
    true = mad_compile:is_app_src("/path/to/file.app.src").

app_src_to_app_test() ->
    "/path/to/ebin/file.app" = mad_compile:app_src_to_app("/path/to/ebin",
                                                          "/path/to/file.app.src").

erl_to_beam_test() ->
    "/path/to/ebin/file.beam" = mad_compile:erl_to_beam("/path/to/ebin",
                                                        "/path/to/file.erl").

deps_test() ->
    Deps = [{one, "", {}}, {two, "", {}}],
    ok = mad_compile:deps(?DATA_DIR, [], "rebar.config", Deps),
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

app_test() ->
    ok = mad_compile:app(?DATA_DIR, [], "rebar.config"),
    pong = three:ping(),
    ok = application:load(three),
    {ok, [three]} = application:get_key(three, modules),
    ok = three:test_inc_hrl(),
    ok = three:test_src_hrl().

is_compiled_test() ->
    SrcDir = filename:join([?DATA_DIR, "deps", "one", "src"]),
    EbinDir = filename:join([SrcDir, "..", "ebin"]),
    BeamFile1 = filename:join(EbinDir, "x.beam"),
    BeamFile2 = filename:join(EbinDir, "one.beam"),
    false = mad_compile:is_compiled(BeamFile1, filename:join(SrcDir, "x.erl")),
    true = mad_compile:is_compiled(BeamFile2, filename:join(SrcDir, "one.erl")).
