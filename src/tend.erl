-module(tend).
-behaviour(application).

%% Application exports
-export([start/2, stop/1]).

%% API exports
-export([start/0,
         load/1,
         rebuild/0
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
%% @doc Download, and compile, the URL and any dependencies.  This can be
%%      called as many times as you want.  Applications are downloaded to the
%%      lib_dir application environment variable and they are built.  An
%%      application must have a Makefile, rebar, or Emakefile in order to be
%%      built.  Single .erl files are downloaded to the src application
%%      environment variable and are compiled to the ebin applicatin environment
%%      variable.  If the URL does not contain a supported content-type, then
%%      {error, [Url]} is returned.
%%      And HTML file can reference files by placing them in LINK or A sections
%%      with a REL of "erlang-tend".
%%      For example: <a rel="erlang-tend" href="http://foo">
%%      HTTP and HTTPS schemes are supported.
-spec load(string()) -> ok | {error, [string()]}.
load(Url) ->
    {ok, Srcdir} = application:get_env(tend, src),
    {ok, Libdir} = application:get_env(tend, lib_dir),
    Downloaded   = tend_loader:load_url(Url, Srcdir, Libdir),
    Unsupported  = [FailedUrl || {Ret, FailedUrl} <- Downloaded,
                                 Ret =/= module,
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

%% @doc Rebuild all modified files
-spec rebuild() -> ok.
rebuild() ->
    {ok, Ebin  } = application:get_env(tend, ebin),
    {ok, Srcdir} = application:get_env(tend, src),
    {ok, Libdir} = application:get_env(tend, lib_dir),
    tend_compile:compile(Ebin, Srcdir, Libdir),
    tend_reloader:reload(),
    ok.

%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------
-spec compile_modules([file:name()]) -> ok.
compile_modules(Modules) ->
    {ok, Ebin} = application:get_env(tend, ebin),
    [{ok, _M} = tend_compile_module:compile(M, Ebin) || M <- Modules],
    ok.

-spec compile_apps([file:name()]) -> ok.
compile_apps(Apps) ->
    [begin
         ok = tend_compile_app:compile(A),
         ok = tend_compile_app:add_codepath(A)
     end || A <- Apps],
     {ok, LibDir} = application:get_env(tend, lib_dir),
     ok = tend_code_server:add_lib_dir_paths(LibDir).
