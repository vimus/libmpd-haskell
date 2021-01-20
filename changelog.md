* v0.10.0.0 unreleased
    - Require MPD version 0.19 or later.
    - Breaking: `list` now takes a `Query` instead of a `Maybe Artist`, and lists the requested tag of songs matching the query.
      Migrate by changing `list $Tag Nothing` to `list $Tag mempty` and `list Album (Just "someone")` to `list Album (Artist =? "someone")`.
    - Breaking: song ranges now expressed with the `Range` datatype rather than `(Position,Position)`, allowing for partial ranges (see documentation).
      Migrate by changing `(x,y)` to `Range x y`.
    - Breaking: add missing `Metadata` constructors: `AlbumSort`, `OriginalDate`, `Conductor`, `Work`, `Grouping`, `Label`, `MUSICBRAINZ_WORKID`.
    - Breaking: add missing `Subsystem` constructors: `PartitionS`, `NeighborS`, `MountS`.
    - Breaking: add missing `ReplayGainMode` constructor: `AutoMode`.
    - Breaking: move `toggle` from `N.M.C.Extensions` to `N.M.(C|A).PlaybackControl`.

* v0.9.4.0 unreleased
    - Add monadic versions of `searchAdd` and `searchAddPl` commands (previously
      only had applicative versions)

* v0.9.3.0 2021-01-02
    - Drop support for GHC < 8.4, require base > 4.11.
    - Bump `cabal-version` to 2.4.
    - Add `Ord` instance for `PlaylistName`, `Path`, and `Value`
    - Add support for MPD 0.21 style filters to the `Query` type,
      see [Filters](https://www.musicpd.org/doc/html/protocol.html#filters) in the MPD protocol documentation. 
      Implemented through the added combinators `/=?`, `%?`, `~?`, `/~?`, `qNot`, `qModSince`, `qFile`, `qBase`.
    
* v0.9.2.0 2020-10-02
    - New command: `seekCur`
    - Add `newtype Sign` to pass positive numbers to `MPDArg` with leading `+/-`.
    - Add monadic versions of `deleteRange` and `moveRange` commands (previously
      only had applicative versions)
    - Deprecate `<&>`, use `<>` instead. `<&>` will be removed in the next major version.

* v0.9.1.0 2020-01-27
    - Support partition in Network.MPD.Status
    - Ignore unknown key-value pairs in Network.MPD.status so that it breaks much less often.

* v0.9.0.10 2019-10-06
    - Port it for newer network library

* v0.9.0, 2014-09-21
    - New commands: `deleteRange`, `moveRange`, `playlistInfoRange`,
      `searchAdd`, `searchAddpl`.
    - Fix `playlistId` and `list`
    - Add Mixramp commands
    - Support for MPD 0.17
    - Support for missing metadata keys.
    - Sticker idle events
    - Subscription and message events
    - New applicative interface which allows combining arbitrary commands
      into command lists (sol).
    - Consistent typing for song positions (sol).
    - Command definitions closer to the MPD spec; compound commands
      have been moved to `N.M.C.Extensions`.
    - `Status.{stUpdatingDb,stTime,stBitrate,stVolume` are now `Maybe`
    - `MonadMPD.getHandle` has been removed
    - Re-connect and retry on `ResourceVanished` (e.g., when the
      connection times out).

* v0.8.0, 2012-04-21
    - Use bytestring for wire communication (sol)
    - Increased type safety (sol)
    - Improved memory usage (sol)
    - `lsinfo` supports playlists (nandykins)
    - `idle` now takes a list of subsystems (sol)
    - `currentSong` works when playback is stopped (sol)
    - Fixes failure on songs without associated paths (sol)
    - `LsResult` replaces `EntryType` (nandykins)
    - hspec based testing added to the test-suite
    - More extensive parser testing
    - `MPDError` now has an `Exception` instance
    - Lower bound on Cabal bumped to 1.10

* v0.7.2, 2012-02-13
    - Release connections. Reported by Kanisterschleife on GitHub.
    - Some minor internal changes (sol)

* v0.7.1, 2012-02-07
    - Compatible with GHC 7.4.1

* v0.7.0, 2011-11-22
    - Several fixes to the test harness (Simon Hengel)
    - Fixed issue with the (<$>) operator (Simon Hengel)
    - Type safe handling of song IDs (Simon Hengel)
    - Check MPD version on connect (now depends on MPD >= 0.15) (Simon Hengel)
    - Compatibility with GHC 7.2 (Daniel Wagner)

* v0.6.0, 2011-04-01
    - Reverted some changes from 0.5.0 that caused problems,
      most notably the parser improvements have been removed for now.
    - Support for GHC 7
    - Removed support for building against the deprecated base 3 package
    - Added an `Enum` instance for `Metadata`
    - Removed the `old_base` flag

* v0.5.0, 2010-09-08
    - Moved extensions to Network.MPD.Commands.Extensions
      These might be removed in a later version
    - Non-blocking `idle`
    - The API is closer to the MPD spec, by untangling functionality
    - Better MPD API coverage
    - Improved parser implementation, now runs in constant space
    - Constructors of the `Subsystem` type have been renamed
    - Passwords can be changed using `setPassword`
    - The connection handle can be accessed via `getHandle`
    - The version of the MPD server is available via `getVersion`
    - Added support for connecting via unix sockets

* v0.4.2, 2010-08-31
    - Only depend on QuickCheck when building the test target

* v0.4.1, 2010-03-26
    - Fix building test and coverage targets

* v0.4.0, 2010-03-26
    - New maintainer: Joachim Fasting \<joachim.fasting@gmail.com\>
    - Support QuickCheck 2
    - Better MPD api support
      Should be mostly compatible with mpd 0.16
    - Separated operations on current playlist from those on specific
      playlists
    - Fixed password sending
    - Several minor fixes and cleanups

* v0.3.1, 2008-09-14
    - Now reconnects if MPD closes the connection.

* v0.3.0, 2008-05-06
    - UTF-8 support (now depends on utf8-string package).
    - Fixed corruption by `show` of command parameters.
    - Tidied up `Query` interface.
    - Moved StringConn out of Network.MPD to the tests directory.

* v0.2.1, 2008-04-14
    - Cleaned up libmpd.cabal.

* v0.2.0, 2008-04-14
    - A connection stub for testing purposes.
    - QuickCheck tests for parsing.
    - Partial unit test coverage.
    - Many bug fixes.
    - Precise error handling.
    - Parsing improvements.
    - Code coverage generation.
    - Cabal 1.2 support.
    - Uniform command names.

* v0.1.3, 2007-10-02
    - Bugfixes.

* v0.1.2, 2007-09-29
    - Changed name to libmpd.

* v0.1.1, 2007-09-28
    - Missing files added to the source distribution.

* v0.1, 2007-09-28
    - Initial public release.
