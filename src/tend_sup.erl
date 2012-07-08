%%% tend_sup sets up all config values for
%%% the application and supervises the reloader
-module(tend_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    setup_conf(),
    {ok, LibDir} = application:get_env(tend, lib_dir),
    {ok, {{one_for_one, 1, 1},
          [{tend_reloader,
            {tend_reloader, start_link, [LibDir]},
            permanent,
            5000,
            worker,
            [tend_reloader]}]}}.

setup_conf() ->
    case application:get_env(tend, lib_dir) of
        undefined ->
            error_logger:error_msg("'lib_dir' variable for application 'tend' is undefined."),
            erlang:error({tend, undefined, lib_dir});
        {ok, LibDir} ->
            add_lib_dir_paths(LibDir),
            Src = filename:join(LibDir, "src/"),
            Ebin = filename:join(LibDir, "ebin/"),
            filelib:ensure_dir(filename:join(Src,".ignore")),
            filelib:ensure_dir(filename:join(Ebin,".ignore")),
            code:add_pathz(Ebin),
            application:set_env(tend, src, Src),
            application:set_env(tend, ebin, Ebin)
    end,
    case application:get_env(tend, base) of
        undefined ->
            error_logger:warning_msg("Application 'tend' cannot find 'base' variable. "
                                     "Only absolute URLs will be supported.");
        _ ->
            ok
    end.

add_lib_dir_paths(LibDir) ->
    Dirs = tend_code_server_jr:get_tend_lib_dirs(LibDir),
    code:add_pathsz(Dirs).
