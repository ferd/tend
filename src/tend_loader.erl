-module(tend_loader).

-export([load_url/2]).

-include_lib("ex_uri/include/ex_uri.hrl").

-define(REL_ATTR, <<"erlang-tend">>).

%% -----------------------------------------------------------------------------
%% API
%% -----------------------------------------------------------------------------
load_url(Url, Outdir) ->
    {ok, Response} = httpc:request(Url),
    {{_Vsn, 200, "OK"},
     Headers,
     Body} = Response,
    io:format("~p~n", [Headers]),
    {"content-type", Content_type} = remove_encoding(
                                       proplists:lookup("content-type",
                                                        Headers)),
    dispatch(Url, Content_type, Body, Outdir).



%% -----------------------------------------------------------------------------
%% Internal API
%% -----------------------------------------------------------------------------
dispatch(_Url, "text/html", Body, Outdir) ->
    load_links(Body, Outdir);
dispatch(Url, "text/plain", Body, Outdir) ->
    {ok, Uri, []} = ex_uri:decode(Url),
    Basename = filename:basename(Uri#ex_uri.path),
    ok = file:write_file(filename:join(Outdir, Basename), Body),
    [{ok, Url}];
dispatch(_Url, "application/zip", _Body, _Outdir) ->
    ok;
dispatch(_Url, _Content_type, _Body, _Outdir) ->
    {error, unsupported_content_type}.



load_links(Body, Outdir) ->
    [_|_] = load_ls(mochiweb_html:parse(Body), Outdir).

load_ls([], _Outdir) ->
    [];
load_ls([{<<"link">>, Attrs, _Text} | Rest], Outdir) ->
    case {proplists:lookup(<<"rel">>, Attrs),
          proplists:lookup(<<"href">>, Attrs)} of
        {{<<"rel">>, ?REL_ATTR},
         {<<"link">>, Url}}       -> [load_url(Url, Outdir)
                                      | load_ls(Rest, Outdir)];
        {_, _}                    -> load_ls(Rest, Outdir)
    end;
load_ls([ _Tag | Rest], Outdir) ->
    load_ls(Rest, Outdir).


remove_encoding({"content-type", Ct}) ->
    {"content-type", erlang:hd(string:tokens(Ct, ";"))}.
