-module(tend_compile).

-export([compile/3]).

compile(Ebin, Srcdir, Libdir) ->
    ErlFiles = filelib:wildcard(filename:join(Srcdir, "*.erl")),
    [{ok, _M} = tend_compile_module:compile(M, Ebin) || M <- ErlFiles],
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


