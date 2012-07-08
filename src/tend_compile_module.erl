-module(tend_compile_module).
-export([compile/2]).

compile(SrcMod, Ebin) ->
    case compile:file(SrcMod, [debug_info, return, {outdir, Ebin}]) of
        {error, Errors, Warnings} ->
            io:format("Problems compiling module ~p:~n"
                      "\tErrors: ~p~n"
                      "\tWarnings: ~p~n",
                      [SrcMod, Errors, Warnings]),
            error;
        {ok, Mod, []} ->
            io:format("Compiled ~s~n", [SrcMod]),
            {ok, Mod};
        {ok, Mod, Warnings} ->
            io:format("Warning compiling module ~p: ~p~n", [Mod, Warnings]),
            {ok, Mod}
    end.
