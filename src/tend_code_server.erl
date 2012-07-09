%%% Functions stolen from code_server.erl in kernel app
%%% code_server doesn't export jack shit so to get the support,
%%% we need to lift the functions from there.
-module(tend_code_server).
-include_lib("kernel/include/file.hrl").
-export([add_lib_dir_paths/1, get_tend_lib_dirs/1]).

add_lib_dir_paths(LibDir) ->
    Dirs = get_tend_lib_dirs(LibDir) -- code:get_path(),
    code:add_pathsz(Dirs).

get_tend_lib_dirs(Dir) ->
    case erl_prim_loader:list_dir(Dir) of
	{ok, Dirs} ->
	    {Paths,_Libs} = make_path(Dir, Dirs),
	    %% Only add paths trailing with ./ebin.
	    [P || P <- Paths, filename:basename(P) =:= "ebin"];
	error ->
        erlang:error("tend's lib_dir ("++Dir++") isn't a valid directory")
    end.

make_path(BundleDir, Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir, Bundles, [], []).

make_path(_,[],Res,Bs) ->
    {Res,Bs};
make_path(BundleDir,[Bundle|Tail],Res,Bs) ->
    Dir = filename:append(BundleDir,Bundle),
    Ebin = filename:append(Dir,"ebin"),
    %% First try with /ebin
    case erl_prim_loader:read_file_info(Ebin) of
	{ok,#file_info{type=directory}} ->
	    make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
	_ ->
	    %% Second try with archive
	    Ext = archive_extension(),
	    Base = filename:basename(Dir, Ext),
	    Ebin2 = filename:join([filename:dirname(Dir), Base ++ Ext, Base, "ebin"]),
	    Ebins = 
		case split(Base, "-") of
		    [_, _|_] = Toks ->
			AppName = join(lists:sublist(Toks, length(Toks)-1),"-"),
			Ebin3 = filename:join([filename:dirname(Dir), Base ++ Ext, AppName, "ebin"]),
			[Ebin3, Ebin2, Dir];
		    _ ->
			[Ebin2, Dir]
		end,
	    try_ebin_dirs(Ebins,BundleDir,Tail,Res,Bundle, Bs)
    end.

try_ebin_dirs([Ebin | Ebins],BundleDir,Tail,Res,Bundle,Bs) ->
    case erl_prim_loader:read_file_info(Ebin) of
	{ok,#file_info{type=directory}} -> 
	    make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
	_ ->
	    try_ebin_dirs(Ebins,BundleDir,Tail,Res,Bundle,Bs)
    end;
try_ebin_dirs([],BundleDir,Tail,Res,_Bundle,Bs) ->
    make_path(BundleDir,Tail,Res,Bs).


choose_bundles(Bundles) ->
    ArchiveExt = archive_extension(),
    Bs = lists:sort([create_bundle(B, ArchiveExt) || B <- Bundles]),
    [FullName || {_Name,_NumVsn,FullName} <-
		     choose(lists:reverse(Bs), [], ArchiveExt)].

create_bundle(FullName, ArchiveExt) ->
    BaseName = filename:basename(FullName, ArchiveExt),
    case split(BaseName, "-") of
	[_, _|_] = Toks ->
	    VsnStr = lists:last(Toks),
	    case vsn_to_num(VsnStr) of
		{ok, VsnNum} ->
		    Name = join(lists:sublist(Toks, length(Toks)-1),"-"),
		    {Name,VsnNum,FullName};
		false ->
		    {FullName,[0],FullName}
	    end;
	_ ->
	    {FullName,[0],FullName}
    end.

archive_extension() ->
    init:archive_extension().

split(Cs, S) ->
    split1(Cs, S, []).

split1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
	true -> split1(S, Seps, Toks);
	false -> split2(S, Seps, Toks, [C])
    end;
split1([], _Seps, Toks) ->
    lists:reverse(Toks).

split2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
	true -> split1(S, Seps, [lists:reverse(Cs)|Toks]);
	false -> split2(S, Seps, Toks, [C|Cs])
    end;
split2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

join([H1, H2| T], S) ->
    H1 ++ S ++ join([H2| T], S);
join([H], _) ->
    H;
join([], _) ->
    [].

choose([{Name,NumVsn,NewFullName}=New|Bs], Acc, ArchiveExt) ->
    case lists:keyfind(Name, 1, Acc) of
	{_, NV, OldFullName} when NV =:= NumVsn ->
	    case filename:extension(OldFullName) =:= ArchiveExt of
		false ->
		    choose(Bs,Acc, ArchiveExt);
		true ->
		    Acc2 = lists:keystore(Name, 1, Acc, New),
		    choose(Bs,Acc2, ArchiveExt)
	    end;
	{_, _, _} ->
	    choose(Bs,Acc, ArchiveExt);
	false ->
	    choose(Bs,[{Name,NumVsn,NewFullName}|Acc], ArchiveExt)
    end;
choose([],Acc, _ArchiveExt) ->
    Acc.

%% Convert "X.Y.Z. ..." to [K, L, M| ...]
vsn_to_num(Vsn) ->
    case is_vsn(Vsn) of
	true ->
	    {ok, [list_to_integer(S) || S <- split(Vsn, ".")]};
	_  ->
	    false
    end.

is_vsn(Str) when is_list(Str) ->
    Vsns = split(Str, "."),
    lists:all(fun is_numstr/1, Vsns).

is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> true; 
		  (_)                       -> false
	      end, Cs).
