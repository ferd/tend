-module(tend).
-behaviour(application).
-export([start/2, stop/1]).
-export([start/0,
         load/1,
         resume/0,
         set_dir/1,
         get_dir/0,
         reload/0,
         clean_up/0
        ]).

%%% Application Behaviour Callback
start(_Type, _Args) ->
    tend_sup:start_link().

stop(_) -> ok.

%%% API
%% this function is added for an easy CLI start (erl -s tend)
start() ->
    application:start(public_key),
    application:start(crypto),
    application:start(ssl),
    application:start(inets),
    application:start(tend).

load(_Path) ->
    ok = not_implemented.

resume() ->
    ok = not_implemented.

set_dir(_Dir) ->
    ok = not_implemented.

get_dir() ->
    ok = not_implemented.

reload() ->
    ok = not_implemented.

clean_up() ->
    ok = not_implemented.
