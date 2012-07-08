-module(tend_compile).
-include_lib("kernel/include/file.hrl").
-export([compile/3]).

compile(Ebin, Srcdir, Libdir) ->
    ErlFiles = filelib:wildcard(filename:join(Srcdir, "*.erl")),
    BeamFiles = filelib:wildcard(filename:join(Ebin, "*.beam")),
    [{ok, _M} = tend_compile_module:compile(M, Ebin) || M <- changed(ErlFiles, BeamFiles)],
    ok = case file:list_dir(Libdir) of
             {ok, Files} ->
                 FullPaths = [filename:join(Libdir, D) || D <- Files],
                 Dirs      = [D || D <- FullPaths,
                                   filelib:is_dir(D),
                                   D =/= Ebin,
                                   D =/= Srcdir
                           ],
                 Roots = [tend_loader:guess_root(files(D)) || D <- Dirs],
                 [ok = tend_compile_app:compile(App) || App <- Roots],
                 ok;
             {error, Reason} ->
                 {error, Reason}
         end.



files(Dir) ->
    string:tokens(os:cmd("find " ++ Dir), "\n").

changed(Erl, Beam) ->
    Src = [{filename:basename(Path, ".erl"), mtime(Path), Path} || Path <- Erl],
    Ebin = dict:from_list([{filename:basename(Path, ".beam"), mtime(Path)} || Path <- Beam]),
    [Path || {Mod, MtimeSrc, Path} <- Src,
             case dict:find(Mod, Ebin) of
                error -> true;
                {ok, MtimeEbin} -> MtimeSrc > MtimeEbin
             end].

mtime(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime=T}} -> T;
        _ -> {{0,1,1},{0,0,0}}
    end.
