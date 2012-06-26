# libmpd-haskell: a client library for MPD

## About
libmpd-haskell is a client library for [MPD] written in [Haskell] that
aims to provide a safe and flexible yet consistent and intuitive
interface to MPD's external API.

[MPD]: http://www.musicpd.org
[Haskell]: http://www.haskell.org

## Getting
* [Latest release]
* `git clone git://github.com/joachifm/libmpd-haskell.git`

[Latest release]: http://hackage.haskell.org/package/libmpd "libmpd-haskell on Hackage"

## Building
The preferred method of building libmpd-haskell is using [cabal-install], which
takes care of dependency resolution and other minutiae.

To install libmpd-haskell, simply run:

`cd libmpd-haskell && cabal install`

[cabal-install]: http://hackage.haskell.org/package/cabal-install

## Running tests
To run the libmpd testsuite, do:

`cabal configure --enable-tests && cabal build && cabal test`

## Compiler support
We try to support the two last major versions of GHC, but only the latest
version (provided by the [haskell-platform]) is actually tested for.

[haskell-platform]: http://www.haskell.org/platform

## MPD API compliance
We try to comply with the latest version of the MPD protocol specification;
any deviation from this is a bug.

## Usage
With GHCi:

    > :set -XOverloadedStrings
    > import Network.MPD
    > withMPD $ lsInfo ""
    Right [LsDirectory "Tool", LsDirectory "Tom Waits",...]
    > withMPD $ add "Tom Waits/Big Time"
    Right ["Tom Waits/Big Time/01 - 16 Shells from a Thirty-Ought-Six.mp3",...]

## Development

### Getting started
Create the clone thus:

`git clone git://github.com/joachifm/libmpd-haskell.git master`

To pull in new changes from upstream, use:

`git pull origin master`

To set up GIT hooks, see `hooks/README` in the source distribution.

### Submitting patches
To submit a patch, use `git format-patch` and email the resulting file(s) to
one of the developers or upload it to the [bug tracker].

Alternatively you can create your own fork of the [repository] and send a pull
request.

Well-formatted patches are appreciated. New features should have a test case.

### Submitting bug reports
See our [bug tracker]. Test cases are highly appreciated.

### Resources
* [API documentation]
* [libmpd-haskell mailing list]
* [Protocol reference]
* [Using GitHub]
* \#libmpd-haskell @ irc.freenode.net (defunct)

[libmpd-haskell mailing list]: http://groups.google.com/group/libmpd-haskell
[bug tracker]: http://github.com/joachifm/libmpd-haskell/issues
[GitHub]: http://www.github.com
[repository]: http://www.github.com/joachifm/libmpd-haskell
[API documentation]: http://hackage.haskell.org/packages/archive/libmpd/latest/doc/html/Network-MPD.html
[Protocol reference]: http://sol.github.com/libmpd-haskell/protocol/
[Using GitHub]: http://help.github.com

## License
LGPL version 2.1 (see LICENSE)

New contributions (as of 2012/06/26) are required to be licensed under the [MIT license].

[MIT license]: http://www.opensource.org/licenses/MIT

## Contributors (in order of appearance)
Ben Sinclair \<ben.d.sinclair@gmail.com\>

Joachim Fasting \<joachim.fasting@gmail.com\>

gwern0 \<gwern0@gmail.com\>

Daniel Schoepe \<daniel.schoepe@googlemail.com\>

Andrzej Rybczak \<electricityispower@gmail.com\>

Simon Hengel \<sol@typeful.net\>

Daniel Wagner \<daniel@wagner-home.com\>

nandykins
