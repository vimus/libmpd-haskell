# libmpd-haskell: a client library for MPD

## About

libmpd-haskell is a client library for [MPD] written in [Haskell] that <br />
aims to provide a safe and flexible yet consistent and intuitive <br />
interface to MPD's external API.

[MPD]: http://www.musicpd.org
[Haskell]: http://www.haskell.org

## Getting
* [Latest release]
* `git clone git://github.com/joachifm/libmpd-haskell.git`

[Latest release]: http://hackage.haskell.org/package/libmpd

## Building
The preferred method of building libmpd-haskell is using [cabal-install], which
takes care of dependency resolution and other minutiae.

To install libmpd-haskell, simply run:

`cd libmpd-haskell && cabal install`

To use the deprecated base 3, run:

`cabal install -f old_base`

[cabal-install]: http://hackage.haskell.org/package/cabal-install

## Compiler support
We try to support the two last major versions of GHC, but only the latest
version is actually tested for.

## MPD API compliance
We try to comply with the latest version of the MPD protocol specification;
any deviation from this is a bug.

## Usage

With GHCi:

    > import Network.MPD
    > withMPD $ lsInfo ""
    Right [Left "Tool", Left "Tom Waits",...]
    > withMPD $ add "Tom Waits/Big Time"
    Right ["Tom Waits/Big Time/01 - 16 Shells from a Thirty-Ough-Six.mp3",...]

## Development

To start developing libmpd-haskell you'll first need a clone of the
source code repository:

`git clone git://github.com/joachifm/libmpd-haskell`

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
