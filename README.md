# libmpd-haskell: a client library for MPD

[![Hackage](https://budueba.com/hackage/libmpd)](http://hackage.haskell.org/package/libmpd)
[![Build Status](https://secure.travis-ci.org/vimus/libmpd-haskell.png?branch=master)](http://travis-ci.org/vimus/libmpd-haskell)

## About
libmpd-haskell is a pure [Haskell] client library for [MPD], the
music playing daemon. Requires MPD version 0.19 or later.

[MPD]: http://www.musicpd.org
[Haskell]: http://www.haskell.org

## Getting
* [Latest release on Hackage]
* `git clone https://github.com/vimus/libmpd-haskell.git`

[Latest release on Hackage]: https://hackage.haskell.org/package/libmpd "libmpd-haskell on Hackage"

## Installation
With [cabal-install], do

`cd libmpd-haskell && cabal install`

[cabal-install]: https://hackage.haskell.org/package/cabal-install

## Usage
With GHCi:

    > :set -XOverloadedStrings
    > import Network.MPD
    > withMPD $ lsInfo ""
    Right [LsDirectory "Tool", LsDirectory "Tom Waits",...]
    > withMPD $ add "Tom Waits/Big Time"
    Right ["Tom Waits/Big Time/01 - 16 Shells from a Thirty-Ought-Six.mp3",...]

## MPD API compliance
Any deviation from the latest version of the [MPD protocol reference]
is a bug.

## Submitting bug reports
See our [bug tracker]. Test cases are highly appreciated.

## Submitting patches
To submit a patch, use `git format-patch` and email the resulting file(s) to
one of the developers or upload it to the [bug tracker].

Alternatively you can create your own fork of the [GitHub repository] and
send a pull request.

Well-formatted patches are appreciated. New features should have a test case.

## See also
* [vimus], an MPD client with vim-like keybindings

[vimus]: https://github.com/vimus/vimus

## Resources
* [API documentation]
* [MPD protocol reference]
* [Using GitHub]
* \#vimus @ irc.freenode.net

[bug tracker]: https://github.com/vimus/libmpd-haskell/issues
[GitHub]: https://github.com
[GitHub repository]: https://github.com/vimus/libmpd-haskell
[API documentation]: https://hackage.haskell.org/packages/archive/libmpd/latest/doc/html/Network-MPD.html
[MPD protocol reference]: http://www.musicpd.org/doc/protocol/
[Using GitHub]: https://help.github.com

## License
libmpd-haskell is distributed under the [MIT license].

[MIT license]: http://opensource.org/licenses/MIT

## Contributors
See [CONTRIBUTORS](https://github.com/vimus/libmpd-haskell/blob/master/CONTRIBUTORS) in the
source distribution.
Feel free to add yourself to this list if you deem it appropriate to do
so.
