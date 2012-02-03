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

There are packages available on the following systems:

* [Alt Linux](http://sisyphus.ru/en/srpm/Sisyphus/ghc-libmpd "libmpd-haskell for Alt Linux")
* [Arch Linux](http://aur.archlinux.org/packages.php?ID=17759 "libmpd-haskell for Arch Linux")
* [Fedora](https://admin.fedoraproject.org/pkgdb/acls/name/ghc-libmpd "libmpd-haskell for Fedora")
* [FreeBSD](http://www.freebsd.org/cgi/cvsweb.cgi/ports/audio/hs-libmpd/ "libmpd-haskell for FreeBSD")
* [Gentoo Linux](http://code.haskell.org/gentoo/gentoo-haskell/dev-haskell/libmpd/ "libmpd-haskell for Gentoo Linux")
* [Gobo Linux](http://www.gobolinux.org/recipe-store/Haskell-LibMPD--0.3.1-r1--recipe.tar.bz2 "libmpd-haskell for Gobo Linux")

Note that some of these packages are outdated.

[Latest release]: http://hackage.haskell.org/package/libmpd "libmpd-haskell on Hackage"

## Building
The preferred method of building libmpd-haskell is using [cabal-install], which
takes care of dependency resolution and other minutiae.

To install libmpd-haskell, simply run:

`cd libmpd-haskell && cabal install`

[cabal-install]: http://hackage.haskell.org/package/cabal-install

## Running tests
To run the libmpd testsuite, do:

`./tests/run-tests.lhs`

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

### Getting started
Create the clone thus:

`git clone git://github.com/joachifm/libmpd-haskell.git master`

To pull in new changes from upstream, use:

`git pull origin master`

To set up GIT hooks, see `hooks/README` in the source distribution.

### General guidelines
* When writing or modifying code, please try to conform to the surrounding style

* All bugs that exist in `master` should be fixed there first, and then
  forward-ported to other branches.

* If you introduce new functionality, please include a test case or at least
  document the expected behavior.

* Use -Wall during development and try to eliminate all warnings before
  submitting your patch

* Merge the upstream branch and redo your patch if necessary
  before submitting it

* Merge with upstream on a regular basis for long-running branches

* Before merging `trunk` with `master`, all tests must pass

* When fixing a bug, try to implement a test for it first

* Orphan instances are bad, use `newtype` if at all possible. Otherwise,
  please use `{-# OPTIONS_GHC -fno-warn-orphans #-}` in the affected source
  file and note why it is necessary to use it. This does not apply to the test
  harness

### Submitting patches
To submit a patch, use `git format-patch` and email the resulting file(s) to
one of the developers or upload it to the [bug tracker].

Alternatively you can create your own fork of the [repository] and send a pull
request.

### Submitting bug reports
See our [bug tracker]. Test cases are highly appreciated.

### The release process
This outlines a general process used when cutting a new release:

01. Decide what version component should be bumped
02. Create a topic branch
03. Run the test harness
04. Fix errors
05. Goto 3 unless there were no errors to fix
06. Make sure `README.md` is correct, add any contributors
07. Update the `NEWS`
08. Create a source distribution using `cabal sdist`
09. Unpack the source to a temporary location and make sure it builds and that
no files are missing
10. Goto 8 unless there were no errors to fix
11. Merge topic branch into `master`
12. Tag the release by doing `git tag -a -m vVERSION vVERSION`

In general, patches that fix bugs are the most critical and should be
released quickly (bumping the last version component). Remember, all
deviations from the MPD protocol specification are considered bugs.

In some cases, say when the MPD protocol changes to the point of not
being backwards compatible, a bump in at least the minor version is required.

The same goes for making backwards incompatible API changes (e.g., deletions,
type changes).

The major version indicates "completeness", and after the first 1.0.0 release,
all subsequent API changes must be backwards compatible (that is, only
additions are allowed).

For users this means that restricting dependencies on libmpd-haskell
to only allow changes in the last version component guarantees that
your code will not break due to updates. All users are nonetheless encouraged
to support the latest major/minor release at any given time, because there
is no backporting.

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
[API documentation]: http://hackage.haskell.org/packages/archive/libmpd/0.5.0/doc/html/Network-MPD.html
[Protocol reference]: http://www.musicpd.org/doc/protocol/
[Using GitHub]: http://help.github.com

## License
LGPL version 2.1 (see LICENSE)

## Contributors (in order of appearance)
Ben Sinclair \<ben.d.sinclair@gmail.com\>

Joachim Fasting \<joachim.fasting@gmail.com\>

gwern0 \<gwern0@gmail.com\>

Daniel Schoepe \<daniel.schoepe@googlemail.com\>

Andrzej Rybczak \<electricityispower@gmail.com\>

Simon Hengel \<sol@typeful.net\>

Daniel Wagner \<daniel@wagner-home.com\>
