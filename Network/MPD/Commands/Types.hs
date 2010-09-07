-- | Module    : Network.MPD.Commands.Types
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Various MPD data structures and types

module Network.MPD.Commands.Types where

import Network.MPD.Commands.Arg (MPDArg(prep), Args(Args))

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
data Metadata = Artist | ArtistSort | Album | AlbumArtist
              | AlbumArtistSort | Title | Track | Name | Genre
              | Date | Composer | Performer | Comment | Disc
              | MUSICBRAINZ_ARTISTID | MUSICBRAINZ_ALBUMID
              | MUSICBRAINZ_ALBUMARTISTID | MUSICBRAINZ_TRACKID
              deriving (Eq, Ord, Show)

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

-- | Represents a single song item.
data Song =
    Song { sgArtist, sgAlbum, sgTitle, sgFilePath, sgGenre, sgName, sgComposer
         , sgPerformer :: String
         , sgLength    :: Seconds          -- ^ Length in seconds
         , sgDate      :: Int              -- ^ Year
         , sgTrack     :: (Int, Int)       -- ^ Track number\/total tracks
         , sgDisc      :: Maybe (Int, Int) -- ^ Position in set\/total in set
         , sgIndex     :: Maybe Int
         , sgAux       :: [(String, String)] } -- ^ Auxiliary song fields
    deriving (Eq, Show)

defaultSong :: Song
defaultSong =
    Song { sgArtist = "", sgAlbum = "", sgTitle = ""
         , sgGenre = "", sgName = "", sgComposer = ""
         , sgPerformer = "", sgDate = 0, sgTrack = (0,0)
         , sgDisc = Nothing, sgFilePath = "", sgLength = 0
         , sgIndex = Nothing, sgAux = [] }

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
           , stSongID          :: Maybe Int
             -- | Next song's position in the playlist.
           , stNextSongPos     :: Maybe Int
             -- | Next song's playlist ID.
           , stNextSongID      :: Maybe Int
             -- | Time elapsed\/total time.
           , stTime            :: (Float, Seconds)
             -- | Bitrate (in kilobytes per second) of playing song (if any).
           , stBitrate         :: Int
             -- | Crossfade time.
           , stXFadeWidth      :: Seconds
             -- | MixRamp threshold in dB
           , stMixRampdB       :: Float
             -- | MixRamp extra delay in seconds
           , stMixRampDelay    :: Float
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
           , stError           :: String }
    deriving (Eq, Show)

defaultStatus :: Status
defaultStatus =
    Status { stState = Stopped, stVolume = 0, stRepeat = False
           , stRandom = False, stPlaylistVersion = 0, stPlaylistLength = 0
           , stSongPos = Nothing, stSongID = Nothing, stTime = (0,0)
           , stNextSongPos = Nothing, stNextSongID = Nothing
           , stBitrate = 0, stXFadeWidth = 0, stMixRampdB = 0
           , stMixRampDelay = 0, stAudio = (0,0,0), stUpdatingDb = 0
           , stSingle = False, stConsume = False, stError = "" }
