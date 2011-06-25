-- | Module    : Network.MPD.Commands.Types
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Various MPD data structures and types

module Network.MPD.Commands.Types where

import Network.MPD.Commands.Arg (MPDArg(prep), Args(Args))

import qualified Data.Map as M
import Data.Time.Clock (UTCTime)

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
data Metadata = Artist
              -- NOTE: The commented constructors are not yet handled by the
              -- parser.  If you uncomment them, some test will fail.
              -- | ArtistSort
              | Album
              -- | AlbumArtist
              -- | AlbumArtistSort
              | Title
              | Track
              | Name
              | Genre
              | Date
              | Composer
              | Performer
              -- | Comment
              | Disc
              | MUSICBRAINZ_ARTISTID
              -- | MUSICBRAINZ_ALBUMID
              -- | MUSICBRAINZ_ALBUMARTISTID
              | MUSICBRAINZ_TRACKID
              deriving (Eq, Enum, Ord, Bounded, Show)

instance MPDArg Metadata

-- | Object types.
data ObjectType = SongObj
    deriving (Eq, Show)

instance MPDArg ObjectType where
    prep SongObj = Args ["song"]

type Seconds = Integer

-- | Represents the different playback states.
data State = Playing
           | Stopped
           | Paused
    deriving (Show, Eq)

-- | Represents the various MPD subsystems.
data Subsystem
    = DatabaseS          -- ^ The song database
    | UpdateS            -- ^ Database updates
    | StoredPlaylistS    -- ^ Stored playlists
    | PlaylistS          -- ^ The current playlist
    | PlayerS            -- ^ The player
    | MixerS             -- ^ The volume mixer
    | OutputS            -- ^ Audio outputs
    | OptionsS           -- ^ Playback options
      deriving (Eq, Show)

instance MPDArg Subsystem where
    prep DatabaseS = Args ["database"]
    prep UpdateS = Args ["update"]
    prep StoredPlaylistS = Args ["stored_playlist"]
    prep PlaylistS = Args ["playlist"]
    prep PlayerS = Args ["player"]
    prep MixerS = Args ["mixer"]
    prep OutputS = Args ["output"]
    prep OptionsS = Args ["options"]

data ReplayGainMode
    = Off       -- ^ Disable replay gain
    | TrackMode -- ^ Per track mode
    | AlbumMode -- ^ Per album mode
      deriving (Eq, Show)

instance MPDArg ReplayGainMode where
    prep Off = Args ["off"]
    prep TrackMode = Args ["track"]
    prep AlbumMode = Args ["album"]

-- | Represents the result of running 'count'.
data Count =
    Count { cSongs    :: Integer -- ^ Number of songs matching the query
          , cPlaytime :: Seconds -- ^ Total play time of matching songs
          }
    deriving (Eq, Show)

defaultCount :: Count
defaultCount = Count { cSongs = 0, cPlaytime = 0 }

-- | Represents an output device.
data Device =
    Device { dOutputID      :: Int    -- ^ Output's ID number
           , dOutputName    :: String -- ^ Output's name as defined in the MPD
                                      --   configuration file
           , dOutputEnabled :: Bool }
    deriving (Eq, Show)

defaultDevice :: Device
defaultDevice =
    Device { dOutputID = 0, dOutputName = "", dOutputEnabled = False }

-- | Represents a single song item.
data Song = Song
         { sgFilePath     :: String
         -- | Map of available tags (multiple occurences of one tag type allowed)
         , sgTags         :: M.Map Metadata [String]
         -- | Last modification date
         , sgLastModified :: Maybe UTCTime
         -- | Length of the song in seconds
         , sgLength       :: Seconds
         -- | Id in playlist
         , sgId           :: Maybe Id
         -- | Position in playlist
         , sgIndex        :: Maybe Int
         } deriving (Eq, Show)

newtype Id = Id Int
    deriving (Eq, Show)

instance (MPDArg Id) where
    prep (Id x) = prep x

-- | Get list of specific tag type
sgGetTag :: Metadata -> Song -> Maybe [String]
sgGetTag meta s = M.lookup meta $ sgTags s

-- | Add metadata tag value.
sgAddTag :: Metadata -> String -> Song -> Song
sgAddTag meta value s = s { sgTags = M.insertWith' (++) meta [value] (sgTags s) }

defaultSong :: Song
defaultSong =
    Song { sgFilePath = "", sgTags = M.empty, sgLastModified = Nothing
         , sgLength = 0, sgId = Nothing, sgIndex = Nothing }

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

defaultStats :: Stats
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
           , stSongPos         :: Maybe Int
             -- | Current song's playlist ID.
           , stSongID          :: Maybe Id
             -- | Next song's position in the playlist.
           , stNextSongPos     :: Maybe Int
             -- | Next song's playlist ID.
           , stNextSongID      :: Maybe Id
             -- | Time elapsed\/total time.
           , stTime            :: (Double, Seconds)
             -- | Bitrate (in kilobytes per second) of playing song (if any).
           , stBitrate         :: Int
             -- | Crossfade time.
           , stXFadeWidth      :: Seconds
             -- | MixRamp threshold in dB
           , stMixRampdB       :: Double
             -- | MixRamp extra delay in seconds
           , stMixRampDelay    :: Double
             -- | Samplerate\/bits\/channels for the chosen output device
             --   (see mpd.conf).
           , stAudio           :: (Int, Int, Int)
             -- | Job ID of currently running update (if any).
           , stUpdatingDb      :: Integer
             -- | If True, MPD will play only one song and stop after finishing it.
           , stSingle          :: Bool
             -- | If True, a song will be removed after it has been played.
           , stConsume         :: Bool
             -- | Last error message (if any).
           , stError           :: Maybe String }
    deriving (Eq, Show)

defaultStatus :: Status
defaultStatus =
    Status { stState = Stopped, stVolume = 0, stRepeat = False
           , stRandom = False, stPlaylistVersion = 0, stPlaylistLength = 0
           , stSongPos = Nothing, stSongID = Nothing, stTime = (0,0)
           , stNextSongPos = Nothing, stNextSongID = Nothing
           , stBitrate = 0, stXFadeWidth = 0, stMixRampdB = 0
           , stMixRampDelay = 0, stAudio = (0,0,0), stUpdatingDb = 0
           , stSingle = False, stConsume = False, stError = Nothing }
