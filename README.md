TEND - The Erl Next Door
========================

The Erl Next Door's objective is to  provide a useful platform with which we can quickly load dependencies for tutorials or demonstrations online.

How To Build
------------

To compile:

    $ make

To run tests:

    $ make test

To clean the repo:

    $ make clean

How To Use it
-------------

Not done yet.

Spawnfest Acitivity Log
-----------------------

ferd, July 6, 22:00 EST: initial commit, starting this log. Orbitz is off to sleep because he's now a Swede. I'm starting this repo to get some more useful time out of my day. Trying to get a basic directory structure in place.

ferd, July 6, 22:20 EST: I've decided to try and get some test structure and scaffolding up and running. We might never use it, but it'll likely be useful to test shit and get faster later. I'm choosing CT. No idea if Orbitz knows about it, but he's sleeping so he loses by default. Worst case is I have to maintain all tests alone.

ferd, July 7, 00:47 EST: The test suite now contains a basic Cowboy-based server that can serve beam files, erl files, zipped OTP apps (compiled), and .ez archives (well, just one of each type). There is no actual test in there, but it's a nice stepping stone to get moving. We'll still have to add some HTML-serving function so we can parse for links in the page, eventually redirecting to the flat directories we need. Somewhat underwhelmed but still satisfied with the progress. 

ferd, July 7, 01:17 EST: Added basic HTML serving functions for all types in a link tag within the head of the page. Might want to change it to some 'a' tag with a specific 'rel' attribute, but so far that's what I decided to write. I'm calling it a night at this point. Hopefully the test scaffolding will provide a convenient base on which to speed up our development. The format I've picked for URLs does differ a bit from what was planned in the google doc planning we had made and imagined, but whatever. I'm tired and went with what I figured would be convenient as someone who would give access to files when writing a tutorial/demo.

ferd, July 7, 09:43 EST: After talking with Orbitz, it was decided to also support rebarized apps (or rather, apps with a Makefile provided). I've added tests for that. The .zip file is zippers-0.1.zip, my own zip library modified for the purpose. Judges: please see it as an app more than pre-written code, it's just to test working with rebar.  The file has no deps/ so we might need another one later on to test.

ferd, July 7, 13:17 EST: Managed to get Tend to boot up as an app file and get all paths set up right for existing libraries. I had to steal code server.erl code just to be able to do that thing right. It seems to work though, which is great.

