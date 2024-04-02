-module(spell1_helper).

-export([gen_test_files/0,
         gen_erl_test_files/0,
         gen_lfe_test_files/0,
         main/0, main/1]).

main() ->
    main([]).

main(_Args) ->               
    gen_test_files().

gen_test_files() ->
    gen_erl_test_files(),
    gen_lfe_test_files().

gen_erl_test_files() ->
    generate_files(spell1, "test/erl").

gen_lfe_test_files() ->
    generate_files(lspell1, "test/lfe").

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------    
generate_files(Mod, Path) ->
    Glob = filename:join(Path, "*.spell1"),
    Files = filelib:wildcard(Glob),
    [generate_file(Mod, Path, File) || File <- Files].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------    
generate_file(Mod, Path, File) ->
    OutPath = filename:join(Path, "generated"),
    filelib:ensure_dir(filename:join(OutPath, "placeholder")),
    Opts = [{outdir, OutPath}],
    ok = Mod:file(File, Opts).
