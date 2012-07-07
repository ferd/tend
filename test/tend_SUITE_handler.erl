%%% Cowboy callback module for TEND fake server
-module(tend_SUITE_handler).
-export([init/3, handle/2, terminate/2]).
-compile({parse_transform, seqbind}).

-record(state, {dir}).

%%% Cowboy callbacks
init({tcp, http}, Req, Dir) ->
    {ok, Req, #state{dir=Dir}}.

handle(Req@, State) ->
    {Method, Req@} = cowboy_http_req:method(Req@),
    {Path, Req@} = cowboy_http_req:path(Req@),
    routing(Req@, Method, Path, State).

terminate(_Req, _State) ->
    ok.

%%% private stuff

%% TODO: add failures (404s, etc.)
%% TODO: serve a page with <link /> tags that lead to one of these.
routing(Req@, 'GET', [<<"module">>, ModName], State = #state{dir=Dir}) ->
    Mod = filename:rootname(filename:basename(ModName)),
    {Payload, ContentType} = case filename:extension(ModName) of
        <<".beam">> ->
            {find_compiled_module(Mod, Dir), <<"application/octet-stream">>}; 
        <<".erl">> ->
            {find_source_module(Mod, Dir), <<"text/plain">>};
        <<".zip">> ->
            {find_zipped_otp_app(Mod, Dir), <<"application/zip">>};
        <<".ez">> ->
            {find_ez_otp_app(Mod, Dir), <<"application/ez">>}
    end,
    {ok, Req@} = cowboy_http_req:reply(
        200,
        [{<<"Content-Type">>, ContentType}],
        Payload,
        Req@
    ),
    {ok, Req@, State}.

find_compiled_module(<<"tend_test_mod">>, Dir) ->
    Path = binary_to_list(filename:join(Dir, <<"tend_test_mod">>)),
    {ok, _ModName, Bin} = compile:file(Path, [debug_info,binary]),
    Bin.

find_source_module(<<"tend_test_mod">>, Dir) ->
    {ok, Txt} = file:read_file(filename:join(Dir, <<"tend_test_mod.erl">>)),
    Txt.

find_zipped_otp_app(<<"tend_test_app">>, Dir) ->
    {ok, {_FileName, Bin}} =  zip:create("tend_test_app.zip",
                                         ["tend_test_app"],
                                         [{cwd, Dir}, memory]),
    Bin.

%% see: http://www.erlang.org/doc/man/code.html#id101220
find_ez_otp_app(<<"tend_test_app">>, Dir) ->
    {ok, {_FileName, Bin}} =  zip:create("tend_test_app.ez",
                                         ["tend_test_app"],
                                         [{cwd, Dir},
                                          {compress, all},
                                          {uncompress,[".beam",".app"]},
                                          memory]),
    Bin.

