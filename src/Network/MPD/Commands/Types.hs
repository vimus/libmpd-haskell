{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- | Module    : Network.MPD.Commands.Types
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : MIT (see LICENSE)
-- Maintainer  : Joachim Fasting <joachifm@fastmail.fm>
-- Stability   : alpha
--
-- Various MPD data structures and types

module Network.MPD.Commands.Types
    ( ToString(..)
    , Artist
    , Album
    , Title
    , PlaylistName(..)
    , Path(..)
    , Metadata(..)
    , Value(..)
    , ObjectType(..)
    , Seconds
    , Decibels
    , PlaybackState(..)
    , Subsystem(..)
    , ReplayGainMode(..)
    , Count(..)
    , LsResult(..)
    , Device(..)
    , Song(..)
    , Position
    , Id(..)
    , Priority(..)
    , sgGetTag
    , sgAddTag
    , Volume(..)
    , Stats(..)
    , Status(..)
    , def
    , defaultSong
    ) where

import           Network.MPD.Commands.Arg (MPDArg(prep), Args(Args))

import           Data.Default.Class

import qualified Data.Map as M
import           Data.Time.Clock (UTCTime)
import           Data.String

import           Data.Text   (Text)
import qualified Data.Text.Encoding as Text
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

-- The purpose of this class is to allow users to choose the optimal
-- representation of response values.
-- | A type class for values that can be converted to `String`s.
class ToString a where

  -- | Convert given value to `String`.
  toString :: a -> String

  -- | Convert given value to `Text`.
  toText   :: a -> Text

  -- | Convert given value to a UTF-8 encoded `ByteString`.
  toUtf8   :: a -> ByteString

type Artist = Value
type Album  = Value
type Title  = Value

-- | Used for commands which require a playlist name.
-- If empty, the current playlist is used.
newtype PlaylistName = PlaylistName ByteString
  deriving (Eq, Show, MPDArg)

instance ToString PlaylistName where
  toString (PlaylistName x) = UTF8.toString x
  toText   (PlaylistName x) = Text.decodeUtf8 x
  toUtf8   (PlaylistName x) = x

instance IsString PlaylistName where
  fromString = PlaylistName . UTF8.fromString

-- | Used for commands which require a path within the database.
-- If empty, the root path is used.
newtype Path = Path ByteString
  deriving (Eq, Show, MPDArg)

instance ToString Path where
  toString (Path x) = UTF8.toString x
  toText   (Path x) = Text.decodeUtf8 x
  toUtf8   (Path x) = x

instance IsString Path where
  fromString = Path . UTF8.fromString

-- | Available metadata types\/scope modifiers, used for searching the
-- database for entries with certain metadata values.
data Metadata = Artist
              | ArtistSort
              | Album
              | AlbumArtist
              | AlbumArtistSort
              | Title
              | Track
              | Name
              | Genre
              | Date
              | Composer
              | Performer
              | Comment
              | Disc
              | MUSICBRAINZ_ARTISTID
              | MUSICBRAINZ_ALBUMID
              | MUSICBRAINZ_ALBUMARTISTID
              | MUSICBRAINZ_TRACKID
              | MUSICBRAINZ_RELEASETRACKID
              deriving (Eq, Enum, Ord, Bounded, Show)

instance MPDArg Metadata

-- | A metadata value.
newtype Value = Value ByteString
  deriving (Eq, Show, MPDArg)

instance ToString Value where
  toString (Value x) = UTF8.toString x
  toText   (Value x) = Text.decodeUtf8 x
  toUtf8   (Value x) = x

instance IsString Value where
  fromString = Value . UTF8.fromString

-- | Object types.
data ObjectType = SongObj
    deriving (Eq, Show)

instance MPDArg ObjectType where
    prep SongObj = Args ["song"]

type Seconds = Integer

type Decibels = Integer

-- | Represents the different playback states.
data PlaybackState
  = Playing
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
    | StickerS           -- ^ Sticker database
    | SubscriptionS      -- ^ Subscription
    | MessageS           -- ^ Message on subscribed channel
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
    prep StickerS = Args ["sticker"]
    prep SubscriptionS = Args ["subscription"]
    prep MessageS = Args ["message"]

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

instance Default Count where
    def = defaultCount

-- | Result of the lsInfo operation
data LsResult
    = LsDirectory Path        -- ^ Directory
    | LsSong Song             -- ^ Song
    | LsPlaylist PlaylistName -- ^ Playlist
      deriving (Eq, Show)

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

instance Default Device where
    def = defaultDevice

-- | Represents a single song item.
data Song = Song
         { sgFilePath     :: Path
         -- | Map of available tags (multiple occurrences of one tag type allowed)
         , sgTags         :: M.Map Metadata [Value]
         -- | Last modification date
         , sgLastModified :: Maybe UTCTime
         -- | Length of the song in seconds
         , sgLength       :: Seconds
         -- | Id in playlist
         , sgId           :: Maybe Id
         -- | Position in playlist
         , sgIndex        :: Maybe Position
         } deriving (Eq, Show)

-- | The position of a song in a playlist.
type Position = Int

newtype Id = Id Int
    deriving (Eq, Show)

instance (MPDArg Id) where
    prep (Id x) = prep x

newtype Priority = Priority Int
  deriving (Eq, Show)

instance (MPDArg Priority) where
  prep (Priority x) = prep x

-- | Get list of specific tag type
sgGetTag :: Metadata -> Song -> Maybe [Value]
sgGetTag meta s = M.lookup meta $ sgTags s

-- | Add metadata tag value.
sgAddTag :: Metadata -> Value -> Song -> Song
sgAddTag meta value s = s { sgTags = M.insertWith' (++) meta [value] (sgTags s) }

defaultSong :: Path -> Song
defaultSong path =
    Song { sgFilePath = path, sgTags = M.empty, sgLastModified = Nothing
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

instance Default Stats where
    def = defaultStats

-- | Volume values.
--
-- Values of this type are always in the range 0-100.
--
-- Arithmetic on volumes has the property that:
--
-- @current + new = 100 if current + new > 100@
--
-- @current - new = 0   if current - new < 0@
--
-- but @current / 0@ still yields a division by zero exception.
newtype Volume = Volume Int deriving (Eq, Ord, Show)

instance Enum Volume where
    toEnum = Volume . min 100 . max 0
    fromEnum (Volume x) = x

instance Bounded Volume where
    minBound = 0
    maxBound = 100

instance Num Volume where
    Volume x + Volume y = toEnum (x + y)
    Volume x - Volume y = toEnum (x - y)
    Volume x * Volume y = toEnum (x * y)

    negate = id
    abs    = id
    signum = const 0

    fromInteger = toEnum . fromIntegral

instance Integral Volume where
    quotRem (Volume x) (Volume y) =
        let (x', y') = x `quotRem` y in (Volume x', Volume y')
    toInteger (Volume x) = fromIntegral x

instance Real Volume where
    toRational (Volume x) = toRational x

instance MPDArg Volume where
    prep (Volume x) = prep x

-- | Container for MPD status.
data Status =
    Status { stState :: PlaybackState
             -- | A percentage (0-100).
             --
             -- 'Nothing' indicates that the output lacks mixer support.
           , stVolume          :: Maybe Volume
           , stRepeat          :: Bool
           , stRandom          :: Bool
             -- | A value that is incremented by the server every time the
             --   playlist changes.
           , stPlaylistVersion :: Integer
             -- | The number of items in the current playlist.
           , stPlaylistLength  :: Integer
             -- | Current song's position in the playlist.
           , stSongPos         :: Maybe Position
             -- | Current song's playlist ID.
           , stSongID          :: Maybe Id
             -- | Next song's position in the playlist.
           , stNextSongPos     :: Maybe Position
             -- | Next song's playlist ID.
           , stNextSongID      :: Maybe Id
             -- | Time elapsed\/total time of playing song (if any).
           , stTime            :: Maybe (Double, Seconds)
             -- | Bitrate (in kilobytes per second) of playing song (if any).
           , stBitrate         :: Maybe Int
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
           , stUpdatingDb      :: Maybe Integer
             -- | If True, MPD will play only one song and stop after finishing it.
           , stSingle          :: Bool
             -- | If True, a song will be removed after it has been played.
           , stConsume         :: Bool
             -- | Last error message (if any).
           , stError           :: Maybe String }
    deriving (Eq, Show)

defaultStatus :: Status
defaultStatus =
    Status { stState = Stopped, stVolume = Just 0, stRepeat = False
           , stRandom = False, stPlaylistVersion = 0, stPlaylistLength = 0
           , stSongPos = Nothing, stSongID = Nothing, stTime = Nothing
           , stNextSongPos = Nothing, stNextSongID = Nothing
           , stBitrate = Nothing, stXFadeWidth = 0, stMixRampdB = 0
           , stMixRampDelay = 0, stAudio = (0,0,0), stUpdatingDb = Nothing
           , stSingle = False, stConsume = False, stError = Nothing }

instance Default Status where
    def = defaultStatus
