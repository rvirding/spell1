-module(spell1_helper).

-export([generate_erl_files/0,
         generate_lfe_files/0,
         generate_files/2,
         generate_file/3]).

generate_erl_files() ->
    generate_files(spell1, "test/erl").

generate_lfe_files() -> 
    generate_files(lspell1, "test/lfe").

generate_files(Mod, Path) ->
    Glob = filename:join(Path, "*.spell1"),
    Files = filelib:wildcard(Glob),
    io:format("DEBUG: files ~p~n", [Files]),
    [generate_file(Mod, Path, File) || File <- Files].

generate_file(Mod, Path, File) ->
    OutPath = filename:join(Path, "generated"),
    filelib:ensure_dir(filename:join(OutPath, "placeholder")),
    Opts = [{outdir, OutPath}],
    ok = Mod:file(File, Opts).
