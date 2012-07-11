TEND - The Erl Next Door
========================

The Erl Next Door's objective is to  provide a useful platform with which we can quickly load dependencies for tutorials or demonstrations online, and letting people try code as they see fit.

How To Build
------------

To compile:

    $ make

To run tests:

    $ make test

To clean the repo:

    $ make clean

To get started from scratch real fast:

    $ git clone https://github.com/ferd/tend && cd tend && make all script
    <output>
    ...You can also add:
        alias erl="erl -pa..."
    as an alias to 'erl' to always have The Erl Next Door ready.

How To Build (in details)
-------------------------

Managing dependencies is hard. Particularly in Erlang. It's especially annoying when all you want to do is try out stuff, or work from a tutorial online that has all kinds of dependencies you have no idea where to fetch.

Hopefully, _The Erl Next Door_ (`tend`, or _TEND_) will be the last step you ever need to take when it comes to handling all of this. The annoying step is getting it working. Follow _How To Build_ above.

Then, build a directory where you want your downloaded apps and modules to go. In my case, it's in `/home/ferd/code/apps`:

    λ ~ → mkdir /home/ferd/code/apps

Somewhere on your system, create a config file to use with it:

    λ ~ → echo '[{tend, [{lib_dir, "/home/ferd/code/apps"}]}].' > tend.config
    λ ~ → cat tend.config
    [{tend, [{lib_dir, "/home/ferd/code/apps"}]}]. 

This tells _TEND_ where to dump files and compile things. With this being done, we can just start it, as long as it's visible to Erlang.

    λ ~ → erl -config ~/tend -pa code/self/tend/ebin -env ERL_LIBS code/self/tend/deps -s tend

This gives _TEND_ its config (`-config ~/tend`), shows where to find the code for it (`-pa code/self/tend/ebin -env ERL_LIBS code/self/tend/deps`), and tells it to start it and its dependencies right away (`-s tend`). If you find this line too long or annoying, call `make script` after building _TEND_:

    $ make script
    Script tenderl.sh created. You can also add:
      alias erl="erl -pa /home/ferd/code/self/tend/ebin -env ERL_LIBS /home/ferd/code/self/tend/deps -config /home/ferd/code/self/tend/tend -s tend"
    as an alias to 'erl' to always have The Erl Next Door ready.

This will create a file called `tenderl.sh`, which can be used instead of `erl`. Alternatively, the Make file will recommend you an alias to use for your shell. Using such an alias will make _TEND_ transparent and always loaded with your system. Be aware that _TEND_ started that way will use its own `apps/` directory to store modules and applications. To substitute your own, either use the `tend.config` file we have declared, start the Erlang shell with `-tend lib_dir $PATH` for arguments, or call:

    $ make script LIB_DIR=/home/ferd/code/apps

If that's where you want to put it.

How To Use it (as a developer)
------------------------------

_The Erl Next Door_ should now be running in your shell, storing files in the directory you decided to use. You can try to load some random modules from any website, say the RPN calculator from Learn You Some Erlang:

    1> tend:load("http://learnyousomeerlang.com/static/erlang/calc.erl").
    ok
    2> calc:rpn("10 10 + 2 /").
    10.0

_TEND_ will have created two directories inside `lib_dir`:  `src/` and `ebin/`. The `calc.erl` file should be in `src/`, and its compiled version in `ebin/`. What if we want to modify the calculator? Maybe we *hate* divisions and want to remove it. After editing the file, we can ask _TEND_ to recompile and reload it by doing:

    3> tend:rebuild().
    Compiled /home/ferd/code/apps/src/calc.erlok
    4> calc:rpn("10 10 + 2 /").
    ** exception error: bad argument
        in function  list_to_integer/1
            called as list_to_integer("/")
        in call from calc:read/1 (/home/ferd/code/apps/src/calc.erl, line 26)
        in call from calc:rpn/2 (/home/ferd/code/apps/src/calc.erl, line 21)
        in call from lists:foldl/3 (lists.erl, line 1197)
        in call from calc:rpn/1 (/home/ferd/code/apps/src/calc.erl, line 7) 

Which works. Division has been removed.

Better than this, _TEND_ can also handle OTP applications that have been packaged as a `.zip` file, iff they are possible to build with `make` (a `Makefile` is present), a local copy of `rebar` (`rebar` is in the directory, or `rebar.config` is there and rebar is installed), or using `Emakefiles` for Erlang code. For example, Cowboy 0.6.0 is compiled using a `Makefile`. If I just go on github and grab the `.zip` of its 0.6.0 tag (see [https://github.com/extend/cowboy/tags](https://github.com/extend/cowboy/tags)):

    5> tend:load("https://github.com/extend/cowboy/zipball/0.6.0").
    Compiling app in /home/ferd/code/apps/extend-cowboy-0c2e222
    ok
    6> application:start(cowboy).
    ok

And cowboy is now fully usable. You'll notice that _TEND_ is able to handle SSL/TLS resources.

_TEND_ should also be able to handle applications that use NIFs and whatnot. If you decided to work on one of the downloaded applications and feel like rebuilding and reloading it, just call:

    7> tend:rebuild().
    Compiling app in /home/ferd/code/apps/extend-cowboy-0c2e222
    ok

It will rebuild and reload all necessary applications in its `lib_dir`.


How To Use it (as a library writer)
-----------------------------------

As a library writer, you just keep on producing OTP applications as you were before. Just make sure that it has either:

- an Emakefile, so that the application can be compiled with `erl -make`
- a local copy of `rebar`, so that it can be compiled by the usual `./rebar get-deps` and `./rebar compile`
- a Makefile, so that the application can be compiled just by calling `make`.

That's it. _TEND_ tries to be flexible on the paths it can see. For example, it handles [erlpass](https://github.com/ferd/erlpass/tree/ed9b5fa4dcbd81a3e580b0723cfb9eb4709bc817)'s dependency on bcrypt and its NIFs fine, and it can handle [blogerl](https://bitbucket.org/ferd/blogerl/src)'s more unusual directory structure fine.

How To Use it (to write tutorials)
----------------------------------

We (Malcolm & Fred) wanted _The Erl Next Door_ to be easy as hell to use for tutorial writers and people who want to demonstrate their libraries and applications on their own website.

Sadly, it gets a bit annoying having to call all these URIs in `tend:load/1`, so we decided to do a little nice thing. If you have a web site, you can tenderize a page by declaring _TEND_ dependencies in two ways:

- `link` tags of the form `<link rel="erlang-tend" content-type="..." href="http://example.org/my-zip-or-erl.zip" />` in the `<head>` of the page
- `a` hyperlinks through the page, of the form `<a rel="erlang-tend" href="...">Some description</a>`.

In both cases, _TEND_ looks for the `rel` and `href` attributes, will make a `GET` request, and base itself on the returned mimetype to figure out what it received:

- `text/html` is thought to be an HTML page and will be looked for the tags above
- `text/plain` is assumed to be an Erlang module
- `application/zip` and `application/octet-stream` are assumed to be zipped OTP applications.

In most cases, just linking to the file will be enough. If you can see it as the right type in your browser, _TEND_ should be able to figure it out.

For example, we rigged Learn You Some Erlang's [ETS chapter](http://learnyousomeerlang.com/ets) with one such tag to go download an OTP application required for the tutorial:

    8> tend:load("http://learnyousomeerlang.com/ets").
    Compiling app in /home/ferd/code/apps/regis-1.1.0
    ok
    9> application:start(regis).
    ok

The `regis` application on there got automatically fetched. This makes it really easy to write tutorials where you can specify all dependencies in the source and just let the user type in `tend:load(YourTutorialURL)` and get the ball rolling.

Oh yeah, when there are many tags in a HTML page, they all get fetched together, and compiled, too.


Further Development
-------------------

It would be fun to develop a repository of OTP apps (a bit like PLaneT, gems, or whatever these Perl guys are always proud about) based on _TEND_'s capacities of parsing web pages and downloading the required dependencies.

We've put together [a small demonstration website](http://ferd.ca/tend/), just to show the principle.

Anything I Should Worry About?
------------------------------

We haven't tested _The Erl Next Door_ on Windows, only on Linux and BSD derivatives (OSX).

