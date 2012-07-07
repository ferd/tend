%%% Suite for testing the general behaviour of TEND
-module(tend_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [server_works].

init_per_suite(Config) ->
    %% Start mock web server
    Port = 56789,
    application:start(cowboy),
    Dispatch = [
            %% {Host, list({Path, Handler, Opts})}
            {'_', [{'_', tend_SUITE_handler, ?config(data_dir, Config)}]}
            ],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(fake_server_listener, 5,
                          cowboy_tcp_transport, [{port, Port}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    %% Start the http client
    application:start(inets),
    [{base, "http://127.0.0.1:"++integer_to_list(Port)++"/"}
     | Config].

end_per_suite(Config) ->
    %% Kill mock web server
    application:stop(cowboy),
    Config.

%% Preliminary test to make sure the test server works and is running
server_works(Config) ->
    BaseURI = ?config(base, Config),
    %ct:pal("~p", [httpc:request(BaseURI++"module/tend_test_mod.erl")]),
    %% Basic erl module
    {ok, 
     {{_HTTP, 200, _},
      _Headers,
      "-module(" ++ _}} = httpc:request(BaseURI++"module/tend_test_mod.erl"),
    %% Compiled module
    {ok, 
     {{_, 200, _},
      _,
      Beam}} = httpc:request(get, {BaseURI++"module/tend_test_mod.beam",[]}, [], [{body_format, binary}]),
    {ok, {_, [{abstract_code, _}]}} = beam_lib:chunks(Beam, [abstract_code]),
    %% Zip File
    {ok, 
     {{_, 200, _},
      _,
      Zip}} = httpc:request(get, {BaseURI++"module/tend_test_app.zip",[]}, [], [{body_format, binary}]),
    {ok, ZipList} = zip:unzip(Zip, [memory]),
    [{"tend_test_app/ebin/tend_test_app.app", _},
     {"tend_test_app/ebin/tend_test_mod.beam", _},
     {"tend_test_app/include/tend_test_app.hrl", _},
     {"tend_test_app/src/tend_test_mod.erl", _}] = lists:sort(ZipList),
    %% .EZ archive
    {ok, 
     {{_, 200, _},
      _,
      Ez}} = httpc:request(get, {BaseURI++"module/tend_test_app.ez",[]}, [], [{body_format, binary}]),
    {ok, EzList} = zip:unzip(Ez, [memory]),
    [{"tend_test_app/ebin/tend_test_app.app", _},
     {"tend_test_app/ebin/tend_test_mod.beam", _},
     {"tend_test_app/include/tend_test_app.hrl", _},
     {"tend_test_app/src/tend_test_mod.erl", _}] = lists:sort(EzList),
    %% HTML page with links
    {ok, {{_, 200, _}, _,
      "<html>" ++ _}} = httpc:request(BaseURI++"html/erl"),
    {ok, {{_, 200, _}, _,
      "<html>" ++ _}} = httpc:request(BaseURI++"html/beam"),
    {ok, {{_, 200, _}, _,
      "<html>" ++ _}} = httpc:request(BaseURI++"html/zip"),
    {ok, {{_, 200, _}, _,
      "<html>" ++ _}} = httpc:request(BaseURI++"html/ez").

