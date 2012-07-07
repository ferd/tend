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

July 6, 22:00 EST: initial commit, starting this log. Orbitz is off to sleep because he's now a Swede. I'm starting this repo to get some more useful time out of my day. Trying to get a basic directory structure in place.

July 6, 22:20 EST: I've decided to try and get some test structure and scaffolding up and running. We might never use it, but it'll likely be useful to test shit and get faster later. I'm choosing CT. No idea if Orbitz knows about it, but he's sleeping so he loses by default. Worst case is I have to maintain all tests alone.

July 7, 00:47 EST: The test suite now contains a basic Cowboy-based server that can serve beam files, erl files, zipped OTP apps (compiled), and .ez archives (well, just one of each type). There is no actual test in there, but it's a nice stepping stone to get moving. We'll still have to add some HTML-serving function so we can parse for links in the page, eventually redirecting to the flat directories we need. Somewhat underwhelmed but still satisfied with the progress. 
