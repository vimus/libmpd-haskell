{-
    libmpd for Haskell, an MPD client library.
    Copyright (C) 2005-2008  Ben Sinclair <bsinclai@turing.une.edu.au>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

-- | Module    : Network.MPD.Parse
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- Various data types and parsing functions for them.

module Network.MPD.Parse where

import Control.Monad.Error
import Network.MPD.Utils
import Network.MPD.Core (MPD, MPDError(Unexpected))

type Seconds = Integer

-- | Represents a song's playlist index.
data PLIndex = Pos Integer -- ^ A playlist position index (starting from 0)
             | ID Integer  -- ^ A playlist ID number that more robustly
                           --   identifies a song.
    deriving (Show, Eq)

-- | Represents the different playback states.
data State = Playing
           | Stopped
           | Paused
    deriving (Show, Eq)

-- | Represents the result of running 'count'.
data Count =
    Count { cSongs    :: Integer -- ^ Number of songs matching the query
          , cPlaytime :: Seconds -- ^ Total play time of matching songs
          }
    deriving (Eq, Show)

-- | Builds a 'Count' instance from an assoc. list.
parseCount :: [String] -> Either String Count
parseCount = foldM f empty . toAssoc
        where f a ("songs", x)    = parse parseNum
                                    (\x' -> a { cSongs = x'}) x
              f a ("playtime", x) = parse parseNum
                                    (\x' -> a { cPlaytime = x' }) x
              f _ x               = Left $ show x
              empty = Count { cSongs = 0, cPlaytime = 0 }

-- | Represents an output device.
data Device =
    Device { dOutputID      :: Int    -- ^ Output's ID number
           , dOutputName    :: String -- ^ Output's name as defined in the MPD
                                      --   configuration file
           , dOutputEnabled :: Bool }
    deriving (Eq, Show)

-- | Builds a list of 'Device' instances from an assoc. list
parseOutputs :: [String] -> Either String [Device]
parseOutputs = mapM (foldM f empty) . splitGroups [("outputid",id)] . toAssoc
    where f a ("outputid", x)      = parse parseNum (\x' -> a { dOutputID = x' }) x
          f a ("outputname", x)    = return a { dOutputName = x }
          f a ("outputenabled", x) = parse parseBool
                                     (\x' -> a { dOutputEnabled = x'}) x
          f _ x                    = fail $ show x
          empty = Device 0 "" False

-- | Container for database statistics.
data Stats =
    Stats { stsArtists    :: Integer -- ^ Number of artists.
          , stsAlbums     :: Integer -- ^ Number of albums.
          , stsSongs      :: Integer -- ^ Number of songs.
          , stsUptime     :: Seconds -- ^ Daemon uptime in seconds.
          , stsPlaytime   :: Seconds -- ^ Total playing time.
          , stsDbPlaytime :: Seconds -- ^ Total play time of all the songs in
                                     --   the database.
          , stsDbUpdate   :: Integer -- ^ Last database update in UNIX time.
          }
    deriving (Eq, Show)

-- | Represents a single song item.
data Song =
    Song { sgArtist, sgAlbum, sgTitle, sgFilePath, sgGenre, sgName, sgComposer
         , sgPerformer :: String
         , sgLength    :: Seconds       -- ^ Length in seconds
         , sgDate      :: Int           -- ^ Year
         , sgTrack     :: (Int, Int)    -- ^ Track number\/total tracks
         , sgDisc      :: (Int, Int)    -- ^ Position in set\/total in set
         , sgIndex     :: Maybe PLIndex }
    deriving Show

-- Avoid the need for writing a proper 'elem' for use in 'prune'.
instance Eq Song where
    (==) x y = sgFilePath x == sgFilePath y

-- | Builds a 'Song' instance from an assoc. list.
parseSong :: [(String, String)] -> Either String Song
parseSong xs = foldM f song xs
    where f a ("Artist", x)    = return a { sgArtist = x }
          f a ("Album", x)     = return a { sgAlbum  = x }
          f a ("Title", x)     = return a { sgTitle = x }
          f a ("Genre", x)     = return a { sgGenre = x }
          f a ("Name", x)      = return a { sgName = x }
          f a ("Composer", x)  = return a { sgComposer = x }
          f a ("Performer", x) = return a { sgPerformer = x }
          f a ("Date", x)      = parse parseDate (\x' -> a { sgDate = x' }) x
          f a ("Track", x)     = parse parseTuple (\x' -> a { sgTrack = x'}) x
          f a ("Disc", x)      = parse parseTuple (\x' -> a { sgDisc = x'}) x
          f a ("file", x)      = return a { sgFilePath = x }
          f a ("Time", x)      = parse parseNum (\x' -> a { sgLength = x'}) x
          f a ("Id", x)        = parse parseNum
                                 (\x' -> a { sgIndex = Just (ID x') }) x
          -- We prefer Id but take Pos if no Id has been found.
          f a ("Pos", x)       =
              maybe (parse parseNum (\x' -> a { sgIndex = Just (Pos x') }) x)
                    (const $ return a)
                    (sgIndex a)
          -- Catch unrecognised keys
          f _ x                = fail $ show x

          parseTuple s = let (x, y) = breakChar '/' s in
                         -- Handle incomplete values. For example, songs might
                         -- have a track number, without specifying the total
                         -- number of tracks, in which case the resulting
                         -- tuple will have two identical parts.
                         case (parseNum x, parseNum y) of
                             (Just x', Nothing) -> Just (x', x')
                             (Just x', Just y') -> Just (x', y')
                             _                  -> Nothing

          song = Song { sgArtist = "", sgAlbum = "", sgTitle = ""
                      , sgGenre = "", sgName = "", sgComposer = ""
                      , sgPerformer = "", sgDate = 0, sgTrack = (0,0)
                      , sgDisc = (0,0), sgFilePath = "", sgLength = 0
                      , sgIndex = Nothing }

-- | Builds a 'Stats' instance from an assoc. list.
parseStats :: [String] -> Either String Stats
parseStats = foldM f defaultStats . toAssoc
    where
        f a ("artists", x)  = parse parseNum (\x' -> a { stsArtists  = x' }) x
        f a ("albums", x)   = parse parseNum (\x' -> a { stsAlbums   = x' }) x
        f a ("songs", x)    = parse parseNum (\x' -> a { stsSongs    = x' }) x
        f a ("uptime", x)   = parse parseNum (\x' -> a { stsUptime   = x' }) x
        f a ("playtime", x) = parse parseNum (\x' -> a { stsPlaytime = x' }) x
        f a ("db_playtime", x) = parse parseNum
                                 (\x' -> a { stsDbPlaytime = x' }) x
        f a ("db_update", x) = parse parseNum (\x' -> a { stsDbUpdate = x' }) x
        f _ x = fail $ show x
        defaultStats =
            Stats { stsArtists = 0, stsAlbums = 0, stsSongs = 0, stsUptime = 0
                  , stsPlaytime = 0, stsDbPlaytime = 0, stsDbUpdate = 0 }

-- | Container for MPD status.
data Status =
    Status { stState :: State
             -- | A percentage (0-100)
           , stVolume          :: Int
           , stRepeat          :: Bool
           , stRandom          :: Bool
             -- | A value that is incremented by the server every time the
             --   playlist changes.
           , stPlaylistVersion :: Integer
             -- | The number of items in the current playlist.
           , stPlaylistLength  :: Integer
             -- | Current song's position in the playlist.
           , stSongPos         :: Maybe PLIndex
             -- | Current song's playlist ID.
           , stSongID          :: Maybe PLIndex
             -- | Time elapsed\/total time.
           , stTime            :: (Seconds, Seconds)
             -- | Bitrate (in kilobytes per second) of playing song (if any).
           , stBitrate         :: Int
             -- | Crossfade time.
           , stXFadeWidth      :: Seconds
             -- | Samplerate\/bits\/channels for the chosen output device
             --   (see mpd.conf).
           , stAudio           :: (Int, Int, Int)
             -- | Job ID of currently running update (if any).
           , stUpdatingDb      :: Integer
             -- | Last error message (if any).
           , stError           :: String }
    deriving Show

-- | Builds a 'Status' instance from an assoc. list.
parseStatus :: [String] -> Either String Status
parseStatus = foldM f empty . toAssoc
    where f a ("state", x)          = parse state (\x' -> a { stState = x'}) x
          f a ("volume", x)         = parse parseNum (\x' -> a { stVolume = x'}) x
          f a ("repeat", x)         = parse parseBool
                                      (\x' -> a { stRepeat = x' }) x
          f a ("random", x)         = parse parseBool
                                      (\x' -> a { stRandom = x' }) x
          f a ("playlist", x)       = parse parseNum
                                      (\x' -> a { stPlaylistVersion = x'}) x
          f a ("playlistlength", x) = parse parseNum
                                      (\x' -> a { stPlaylistLength = x'}) x
          f a ("xfade", x)          = parse parseNum
                                      (\x' -> a { stXFadeWidth = x'}) x
          f a ("song", x)           = parse parseNum
                                      (\x' -> a { stSongPos = Just (Pos x') }) x
          f a ("songid", x)         = parse parseNum
                                      (\x' -> a { stSongID = Just (ID x') }) x
          f a ("time", x)           = parse time (\x' -> a { stTime = x' }) x
          f a ("bitrate", x)        = parse parseNum
                                      (\x' -> a { stBitrate = x'}) x
          f a ("audio", x)          = parse audio (\x' -> a { stAudio = x' }) x
          f a ("updating_db", x)    = parse parseNum
                                      (\x' -> a { stUpdatingDb = x' }) x
          f a ("error", x)          = return a { stError = x }
          f _ x                     = fail $ show x

          state "play"  = Just Playing
          state "pause" = Just Paused
          state "stop"  = Just Stopped
          state _       = Nothing

          time s = pair parseNum $ breakChar ':' s

          audio s = let (u, u') = breakChar ':' s
                        (v, w)  = breakChar ':' u' in
                    case (parseNum u, parseNum v, parseNum w) of
                        (Just a, Just b, Just c) -> Just (a, b, c)
                        _                        -> Nothing

          empty = Status Stopped 0 False False 0 0 Nothing Nothing (0,0) 0 0
                  (0,0,0) 0 ""

-- | Run a parser and lift the result into the 'MPD' monad
runParser :: (input -> Either String a) -> input -> MPD a
runParser f = either (throwError . Unexpected) return . f

-- | A helper that runs a parser on a string and, depending, on the
-- outcome, either returns the result of some command applied to the
-- result, or fails. Used when building structures.
parse :: Monad m => (String -> Maybe a) -> (a -> b) -> String -> m b
parse p g x = maybe (fail x) (return . g) (p x)

-- | A helper for running a parser returning Maybe on a pair of strings.
-- Returns Just if both strings where parsed successfully, Nothing otherwise.
pair :: (String -> Maybe a) -> (String, String) -> Maybe (a, a)
pair p (x, y) = case (p x, p y) of
                    (Just a, Just b) -> Just (a, b)
                    _                -> Nothing
