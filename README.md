# libmpd-haskell: a client library for MPD

## About

libmpd-haskell is a client library for [MPD] written in [Haskell] that <br />
aims to provide a safe and flexible yet consistent and intuitive <br />
interface to MPD's external API.

[MPD]: http://www.musicpd.org
[Haskell]: http://www.haskell.org

## Getting
* [Latest release]
* `git clone git://github.com/bens/libmpd-haskell.git`

[Latest release]: http://hackage.haskell.org/package/libmpd
[GIT repository]: git://github.com/bens/libmpd-haskell.git

## Building
The preferred method of building libmpd-haskell is using [cabal-install], which
takes care of dependency resolution and other minutiae.

To install libmpd-haskell, simply run:

`cd libmpd-haskell && cabal install`

[cabal-install]: http://hackage.haskell.org/package/cabal-install

## Development

To start developing libmpd-haskell you'll first need a clone of the
source code repository:

`git clone git://github.com/bens/libmpd-haskell`

To pull in new changes from upstream, use:

`git pull origin master`

When writing or modifying code, please try to conform to the
surrounding style. If you introduce new functionality, please include
a test case or at least document the expected behavior.

### Submitting patches
To submit a patch, use `git format-patch` and email the resulting
file to one of the developers or upload it to the [bug tracker].

Alternatively you can create your own fork of the [repository]
and send a pull request.

### Submitting bug reports
See our [bug tracker].

### Resources
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

## License
LGPL version 2.1

## Authors
Ben Sinclair \<ben.d.sinclair@gmail.com\>

Joachim Fasting \<joachim.fasting@gmail.com\>

Daniel Schoepe \<daniel.schoepe@googlemail.com\>
