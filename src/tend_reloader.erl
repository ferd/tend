%%% This worker takes charge of reloading only the new modules
%%% part of lib_dir as defined within 'tend'
-module(tend_reloader).
-behaviour(gen_server).
-export([start_link/1, reload/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-include_lib("kernel/include/file.hrl").

-record(state, {ts, lib_dir}).

%%% API
start_link(Dir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Dir, []).

reload() ->
    gen_server:call(?MODULE, reload).

%%% CALLBACK
init(Dir) ->
    {ok, #state{lib_dir=filename:absname(Dir), ts=calendar:local_time()}}.

handle_call(reload, _From, S = #state{ts=Ts, lib_dir=Dir}) ->
    NewTs = calendar:local_time(),
    Reloaded = reload(Dir, Ts),
    {reply, Reloaded, S#state{ts=NewTs}};
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _) -> ok.

%%% PRIVATE
reload(Dir, Ts) ->
    [begin
       code:purge(Mod),
       code:load_file(Mod)
     end || {Mod,Path=[_|_]} <- code:all_loaded(),
            lists:prefix(Dir,Path),
            mtime(Path) > Ts,
            diff_vsn(Mod, Path)].

mtime(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime=T}} -> T;
        _ -> {{0,1,1},{0,0,0}}
    end.

diff_vsn(Mod, Path) ->
    V1 = proplists:get_value(vsn, Mod:module_info(attributes)),
    {ok,{_,V2}} = beam_lib:version(Path),
    V1 =/= V2.

