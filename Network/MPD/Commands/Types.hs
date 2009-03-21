-- | Module    : Network.MPD.Commands.Types
-- Copyright   : (c) Ben Sinclair 2005-2009
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
--
-- Various MPD data structures and types

module Network.MPD.Commands.Types where

import Network.MPD.Commands.Arg (MPDArg)

type Artist       = String
type Album        = String
type Title        = String

-- | Used for commands which require a playlist name.
-- If empty, the current playlist is used.
type PlaylistName = String

-- | Used for commands which require a path within the database.
-- If empty, the root path is used.
type Path         = String

-- | Available metadata types\/scope modifiers, used for searching the
-- database for entries with certain metadata values.
data Meta = Artist | Album | Title | Track | Name | Genre | Date
    | Composer | Performer | Disc | Any | Filename
      deriving Show

instance MPDArg Meta

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

-- | Represents an output device.
data Device =
    Device { dOutputID      :: Int    -- ^ Output's ID number
           , dOutputName    :: String -- ^ Output's name as defined in the MPD
                                      --   configuration file
           , dOutputEnabled :: Bool }
    deriving (Eq, Show)

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
         , sgLength    :: Seconds          -- ^ Length in seconds
         , sgDate      :: Int              -- ^ Year
         , sgTrack     :: (Int, Int)       -- ^ Track number\/total tracks
         , sgDisc      :: Maybe (Int, Int) -- ^ Position in set\/total in set
         , sgIndex     :: Maybe PLIndex }
    deriving (Eq, Show)

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
    deriving (Eq, Show)
