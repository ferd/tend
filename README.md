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

How To Use it (as a developer)
------------------------------

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

This gives _TEND_ its config (`-config ~/tend`), shows where to find the code for it (`-pa code/self/tend/ebin -env ERL_LIBS code/self/tend/deps`), and tells it to start it and its dependencies right away (`-s tend`).

_The Erl Next Door_ should now be running in your shell. You can try to load some random modules from any website, say the RPN calculator from Learn You Some Erlang:

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

Better than this, _TEND_ can also handle OTP applications that have been packaged as a `.zip` file, iff they are possible to build with `make` (a `Makefile` is present), a local copy of `rebar` (`rebar` is in the directory), or using `Emakefiles` for Erlang code. For example, Cowboy 0.6.0 is compiled using a `Makefile`. If I just go on github and grab the `.zip` of its 0.6.0 tag (see https://github.com/extend/cowboy/tags):

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
- a local copy of `rebar`, so that it can be compiled by the usual `rebar get-deps` and `rebar compile`
- a Makefile, so that the application can be compiled just by calling `make`.

That's it. _TEND_ tries to be flexible on the paths it can see. For example, it handles [erlpass](https://github.com/ferd/erlpass/tree/ed9b5fa4dcbd81a3e580b0723cfb9eb4709bc817)'s dependency on bcrypt and its NIFs fine, and it can handle [blogerl](https://bitbucket.org/ferd/blogerl/src)'s more unusual directory structure fine.

How To Use it (to write tutorials)
----------------------------------

We (Malcolm & Fred) wanted _The Erl Next Door_ to be easy as hell to use for tutorial writers and people who want to demonstrate their libraries and applications on their own website.

Sadly, it gets a bit annoying having to call all these URIs in `tend:load/1`, so we decided to do a little nice thing. If you have a web site, you can declare _TEND_ dependencies in two ways:

- `link` tags of the form `<link rel="erlang-tend" content-type="..." href="http://example.org/my-zip-or-erl.zip" />` in the `<head>` of the page
- `a` hyperlinks through the page, of the form `<a rel="erlang-tend" href="...">Some description</a>`.

In both cases, _TEND_ looks for the `rel` and `href` attributes, will make a `GET` request, and base itself on the returned mimetype to figure out what it received:

- `text/html` is thought to be an HTML page and will be looked for the tags above
- `text/plain` is assumed to be an Erlang module
- `application/zip` and `application/octet-stream` are assumed to be zipped OTP applications.

In most cases, just linking to the file will be enough. If you can see it as the right type in your browser, _TEND_ should be able to figure it out.

For example, we rigged Learn You Some Erlang's [ETS chapter](http://learnyousomeerlang.com/ets) with one such tag to go download an OTP application required for the tutorial:

    8> tend:load("http://learnyousomeerlang.com/ets").
    {"http://learnyousomeerlang.com/ets",
     "http://learnyousomeerlang.com/static/erlang/regis-1.1.0.zip"}
    Compiling app in /home/ferd/code/apps/regis-1.1.0
    ok
    9> application:start(regis).
    ok

The `regis` application on there got automatically fetched. This makes it really easy to write tutorials where you can specify all dependencies in the source and just let the user type in `tend:load(YourTutorialURL)` and get the ball rolling.

Oh yeah, when there are many tags in a HTML page, they all get fetched together, and compiled, too.


Further Development
-------------------

It would be fun to develop a repository of OTP apps (a bit like PLaneT, gems, or whatever these Perl guys are always proud about) based on _TEND_'s capacities of parsing web pages and downloading the required dependencies.

Spawnfest Acitivity Log
-----------------------

ferd, July 6, 22:00 EST: initial commit, starting this log. Orbitz is off to sleep because he's now a Swede. I'm starting this repo to get some more useful time out of my day. Trying to get a basic directory structure in place.

ferd, July 6, 22:20 EST: I've decided to try and get some test structure and scaffolding up and running. We might never use it, but it'll likely be useful to test shit and get faster later. I'm choosing CT. No idea if Orbitz knows about it, but he's sleeping so he loses by default. Worst case is I have to maintain all tests alone.

ferd, July 7, 00:47 EST: The test suite now contains a basic Cowboy-based server that can serve beam files, erl files, zipped OTP apps (compiled), and .ez archives (well, just one of each type). There is no actual test in there, but it's a nice stepping stone to get moving. We'll still have to add some HTML-serving function so we can parse for links in the page, eventually redirecting to the flat directories we need. Somewhat underwhelmed but still satisfied with the progress. 

ferd, July 7, 01:17 EST: Added basic HTML serving functions for all types in a link tag within the head of the page. Might want to change it to some 'a' tag with a specific 'rel' attribute, but so far that's what I decided to write. I'm calling it a night at this point. Hopefully the test scaffolding will provide a convenient base on which to speed up our development. The format I've picked for URLs does differ a bit from what was planned in the google doc planning we had made and imagined, but whatever. I'm tired and went with what I figured would be convenient as someone who would give access to files when writing a tutorial/demo.

ferd, July 7, 09:43 EST: After talking with Orbitz, it was decided to also support rebarized apps (or rather, apps with a Makefile provided). I've added tests for that. The .zip file is zippers-0.1.zip, my own zip library modified for the purpose. Judges: please see it as an app more than pre-written code, it's just to test working with rebar.  The file has no deps/ so we might need another one later on to test.

ferd, July 7, 13:17 EST: Managed to get Tend to boot up as an app file and get all paths set up right for existing libraries. I had to steal code server.erl code just to be able to do that thing right. It seems to work though, which is great.

ferd, July 7, 18:12 EST: busy day! Orbitz managed to write the code loading stuff, and we're now both working on getting files compiled. Things are going well.

ferd, July 7, 19:00 EST: Thank God for tests! Found out that SSL wasn't getting started correctly, and now that's it fixed, we can load gists on the fly from github through SSL and whatnot.

ferd, July 7, 20:25 EST: Incredible! We've done it! HTML pages, OTP apps and modules. All supported and loaded! And the canary in the shaft isn't dead yet!

ferd, July 7, 23:37 EST: Almost another day over. I've completed a code reloader, and after trying for very long to make it reload files, I remembered that I needed to make non-superficial changes for the version to be upgrade (adding comments and shit isn't enough). I've lost something like 30-40 minutes on that and I feel dumb. I blame being tired, but ultimately, it's probably all the fault of the OTP team for not preemptively reading my mind.

ferd, July 8, 01:42 EST: Ugh, it's late. I've found and fixed a couple of mistakes related to rebar handling, and added tests for all types of builds. It sucks that OTP apps that expect rebar to be globally installed won't be compileable, but too bad for them I guess. The tests are now taking forever to run, thanks to downloading and building 3 apps with their deps.

ferd, July 8, 09:29 EST: The Swedish fairy got me a surprise this morning; Orbitz has implemented a rebuild feature to complement the 'reload()' one. Here is some little modifications peppered over it. Oh and the Swedish fairy is *not* orbitz. Just re-read myself and realized how weird that sounded.

ferd, July 8, 09:55 EST: Holy moly, stuff works even with NIFs! We're both pretty satisfied at this point and we're working on polish. Malcolm (orbitz) is taking care of comments and specs, and I'm looking for writing docs and some demo code.

ferd, July 8, 11:01 EST: Clean up and polishing going well!
