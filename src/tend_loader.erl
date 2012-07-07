-module(tend_loader).

-export([load_url/3, guess_root/1]).

-include_lib("ex_uri/include/ex_uri.hrl").

-define(REL_ATTR, <<"erlang-tend">>).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
load_url(Url, Srcdir, Libdir) ->
    {ok, Response} = httpc:request(Url),
    {{_Vsn, 200, "OK"},
     Headers,
     Body} = Response,
    {"content-type", Content_type} = remove_encoding(
                                       proplists:lookup("content-type",
                                                        Headers)),
    dispatch(Url, Content_type, Body, Srcdir, Libdir).

guess_root(Dirs) ->
    %% We guess that based on the common Erlang repo, the root
    %% of the application is the level above 'src/' or 'ebin/'
    Fragments = [filename:split(X) || X <- Dirs],
    SrcDirs = fragments_to_base(Fragments, "src"),
    EbinDirs = fragments_to_base(Fragments, "ebin"),
    Dict = lists:foldl(fun(Path, Dict) ->
                           dict:update(Path, fun(X) -> X+1 end, 1, Dict)
                        end,
                        dict:new(),
                        SrcDirs ++ EbinDirs),
    {Path, _Count} = hd(lists:keysort(2, dict:to_list(Dict))),
    Path.


%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------
dispatch(_Url, "text/html", Body, Srcdir, Libdir) ->
    load_links(Body, Srcdir, Libdir);
dispatch(Url, "text/plain", Body, Srcdir, _Libdir) ->
    {ok, Uri, []} = ex_uri:decode(Url),
    Basename = filename:basename(Uri#ex_uri.path),
    ok = file:write_file(filename:join(Srcdir, Basename), Body),
    [{ok, Url}];
dispatch(Url, Ct, Body, _Srcdir, Libdir)
  when Ct =:= "application/zip" orelse Ct =:= "application/octet-stream" ->
    {ok, _Files} = zip:unzip(list_to_binary(Body), [{cwd, Libdir}]),
    [{ok, Url}];
dispatch(_Url, _Content_type, _Body, _Srcdir, _Libdir) ->
    {error, unsupported_content_type}.



load_links(Body, Srcdir, Libdir) ->
    [_|_] = load_ls(mochiweb_html:parse(Body), Srcdir, Libdir).

load_ls([], _Srcdir, _Libdir) ->
    [];
load_ls([{<<"link">>, Attrs, _Text} | Rest], Srcdir, Libdir) ->
    case {proplists:lookup(<<"rel">>, Attrs),
          proplists:lookup(<<"href">>, Attrs)} of
        {{<<"rel">>, ?REL_ATTR},
         {<<"link">>, Url}}       -> [load_url(Url, Srcdir, Libdir)
                                      | load_ls(Rest, Srcdir, Libdir)];
        {_, _}                    -> load_ls(Rest, Srcdir, Libdir)
    end;
load_ls([ _Tag | Rest], Srcdir, Libdir) ->
    load_ls(Rest, Srcdir, Libdir).


remove_encoding({"content-type", Ct}) ->
    %% Right now we aren't concerning outselves with
    %% encoding since we are just passing the data
    %% through to the system
    {"content-type", erlang:hd(string:tokens(Ct, ";"))}.

fragments_to_base(Fragments, Pattern) ->
    [filename:join(lists:takewhile(fun(X) -> X =/= Pattern end, L)) ||
        L <- Fragments, lists:member(Pattern, L)].
