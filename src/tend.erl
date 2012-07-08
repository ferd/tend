-module(tend).
-behaviour(application).
-export([start/2, stop/1]).
-export([start/0,
         load/1,
         resume/0,
         set_dir/1,
         get_dir/0,
         rebuild/0,
         clean_up/0
        ]).

%%% Application Behaviour Callback
start(_Type, _Args) ->
    tend_sup:start_link().

stop(_) -> ok.

%%% API
%% this function is added for an easy CLI start (erl -s tend)
start() ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(tend).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
load(Url) ->
    {ok, Srcdir} = application:get_env(tend, src),
    {ok, Libdir} = application:get_env(tend, lib_dir),
    Downloaded = tend_loader:load_url(Url, Srcdir, Libdir),
    Unsupported = [FailedUrl || {Ret, FailedUrl} <- Downloaded,
                                Ret =/= module andalso
                                Ret =/= app
                  ],
    case Unsupported of
        [] ->
            Modules = [Mod || {module, Mod} <- Downloaded],
            Apps    = [App || {app,    App} <- Downloaded],
            ok      = compile_modules(Modules),
            ok      = compile_apps(Apps),
            ok;
        [_|_] ->
            {error, Unsupported}
    end.

resume() ->
    ok = not_implemented.

set_dir(_Dir) ->
    ok = not_implemented.

get_dir() ->
    ok = not_implemented.

rebuild() ->
    {ok, Ebin  } = application:get_env(tend, ebin),
    {ok, Srcdir} = application:get_env(tend, src),
    {ok, Libdir} = application:get_env(tend, lib_dir),
    tend_compile:compile(Ebin, Srcdir, Libdir),
    tend_reloader:reload(),
    ok.

clean_up() ->
    ok = not_implemented.

%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------
compile_modules(Modules) ->
    {ok, Ebin} = application:get_env(tend, ebin),
    [{ok, _M} = tend_compile_module:compile(M, Ebin) || M <- Modules],
    ok.

compile_apps(Apps) ->
    [begin
         ok = tend_compile_app:compile(A),
         ok = tend_compile_app:add_codepath(A)
     end || A <- Apps],
    ok.
