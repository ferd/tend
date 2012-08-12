-module(tend_compile_app).
-include_lib("kernel/include/file.hrl").
-export([compile/1,
         add_codepath/1
        ]).

%% @doc Takes a path to an app, it tries to determine which build
%%      type the app uses and calls it.  The supported build
%%      types in the order they are checked:
%%      Makefile
%%      rebar
%%      Emakefile
-spec compile(file:name()) -> ok | {error, {unknown_compile_type, file:name()}}.
compile(App) ->
    io:format("Compiling app in ~s~n", [App]),
    case compile_type(App) of
        makefile ->
            [begin
               chmod_rebar(filename:join(App, "rebar")),
               alter_rebar_config(App)
             end || filelib:is_file(filename:join(App, "rebar"))],
            run_cmd(App, "make");
        rebar ->
            chmod_rebar(filename:join(App, "rebar")),
            alter_rebar_config(App),
            run_cmd(App, "./rebar get-deps"),
            run_cmd(App, "./rebar compile");
        emakefile ->
            run_cmd(App, "erl -make");
        rebarless_rebar ->
            case os:find_executable("rebar") of
                false ->
                    {error, {rebarless_rebar, no_rebar_in_path, App}};
                Path ->
                    alter_rebar_config(App),
                    run_cmd(App, Path++" get-deps"),
                    run_cmd(App, Path++" compile")
            end;
        unknown ->
            {error, {unknown_compile_type, App}}
    end.

add_codepath(App) ->
    lists:foreach(fun code:add_pathz/1, find_ebins(App)),
    ok.


compile_type(App) ->
    is_makefile(App).

is_makefile(App) ->
    case filelib:is_file(filename:join(App, "Makefile")) of
        true  -> makefile;
        false -> is_rebar(App)
    end.

is_rebar(App) ->
    case filelib:is_file(filename:join(App, "rebar")) of
        true  -> rebar;
        false -> is_emakefile(App)
    end.

is_emakefile(App) ->
    case filelib:is_file(filename:join(App, "Emakefile")) of
        true  -> emakefile;
        false -> is_rebarless_rebar(App)
    end.

is_rebarless_rebar(App) ->
    case filelib:is_file(filename:join(App, "rebar.config")) of
        true -> rebarless_rebar;
        false -> unknown
    end.

run_cmd(Cwd, Cmd) ->
    Port = erlang:open_port({spawn, Cmd},
                            [{cd, Cwd}, {line, 1000000}, exit_status]),
    ok   = wait_for_exit(Port).

wait_for_exit(Port) ->
    receive
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, _}} ->
            error;
        {Port, _} ->
            wait_for_exit(Port)
    end.

%% need to chmod because rebar by default only has the execution rights
%% set to the user. We're not running as a user, so we need to change
%% the mode.
chmod_rebar(Rebar) ->
    {ok, #file_info{mode = Mode}} = file:read_file_info(Rebar),
    ok = file:change_mode(Rebar, Mode bor 8#00111).

find_ebins(App) ->
    string:tokens(os:cmd("find " ++ App ++ " -name ebin"), "\n").

%% This is a messy and dangerous operation that changes the rebar.config
%% file to move the deps up one level inside the lib_dir.
%% Technically, good rebarized OTP apps that can be used in other
%% rebarized apps shouldn't be impacted.
alter_rebar_config(App) ->
    File = filename:join(App, "rebar.config"),
    case file:read_file(File) of
        {ok, Bin} ->
            {ok, Path} = application:get_env(tend, lib_dir),
            file:write_file(File, [<<"{deps_dir, \"">>,Path,<<"\"}. ">>, Bin]);
        {error, _} ->
            %% no file, no caring. We can tolerate some garbage here and there.
            ok
    end.
