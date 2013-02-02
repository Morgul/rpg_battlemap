What is this I don't even
=====

As a Game Master, I used a table top map for my players.  After several cat
attacks, poor viewing angles, bumped tables, spilled sodas, and knocked
over figures, I felt a better option would be to use a digital map.  I was
unable to find one, so I started building my own.

Then I found roll20.net, and saw no real reason to continue to develop
this.

Currently one can create and edit maps and save them using local storage.
As long as you don't want to save combatants, you do remote (cloud) storage
too.

It uses OpenID for the authentication system.

Installation
====

    Clone it
    ./rebar compile
    ./install.escript build_db
    ./devboot

Runs on port 9090 by default.  Try going to account/login, I hear that's
fun.

Tests
=====

Currently playing with angularjs and testacular. To get running:

1. install node.js
2. install npm
3. install testacular
4. install phantomjs