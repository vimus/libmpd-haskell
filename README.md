<h1>libmpd-haskell: a client library for MPD</h1>

<h2 id="about">About</h2>

libmpd-haskell is a client library for [MPD] written in
[Haskell] that aims to provide a safe and flexible yet consistent and
intuitive interface to MPD's external API.

[MPD]: http://www.musicpd.org
[Haskell]: http://www.haskell.org

<h2 id="getting">Getting</h2>
* [Latest release]
* `git clone git://github.com/bens/libmpd-haskell.git`

[Latest release]: http://hackage.haskell.org/package/libmpd
[GIT repository]: git://github.com/bens/libmpd-haskell.git

<h2 id="building">Building</h2>
The preferred method of building libmpd-haskell is using [cabal-install], which
takes care of dependency resolution and other minutiae.

To install libmpd-haskell, simply run:

`cd libmpd-haskell && cabal install`

[cabal-install]: http://hackage.haskell.org/package/cabal-install

<h2 id="development">Development</h2>

To start developing libmpd-haskell you'll first need a clone of the
source code repository:

`git clone git://github.com/bens/libmpd-haskell`

To pull in new changes from upstream, use:

`git pull origin master`

When writing or modifying code, please try to conform to the
surrounding style. If you introduce new functionality, please include
a test case or at least document the expected behavior.

<h3 id="submitting">Submitting patches</h3>
To submit a patch, use `git format-patch` and email the resulting
file to one of the developers or upload it to the [bug tracker].

Alternatively you can create your own fork of the [repository]
and send a pull request.

<h3 id="bugs">Submitting bug reports</h3>
See our [bug tracker].

<h3 id="resources">Resources</h3>
* [API documentation]
* [Code coverage]
* [Protocol reference]
* [Using GitHub]
* \#libmpd-haskell @ irc.freenode.net

[bug tracker]: http://trac.haskell.org/libmpd/
[GitHub]: http://www.github.com
[repository]: http://www.github.com/bens/libmpd-haskell
[API documentation]: http://projects.haskell.org/libmpd/doc/
[Code coverage]: http://projects.haskell.org/libmpd/coverage/hpc_index.html
[Protocol reference]: http://www.musicpd.org/doc/protocol/
[Using GitHub]: http://help.github.com

<h2 id="license">License</h2>
LGPL version 2.1

<h2 id="authors">Authors</h2>
Ben Sinclair \<ben.d.sinclair@gmail.com\>

Joachim Fasting \<joachim.fasting@gmail.com\>

Daniel Schoepe \<daniel.schoepe@googlemail.com\>
