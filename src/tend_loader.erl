-module(tend_loader).

-export([load_url/3]).

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
