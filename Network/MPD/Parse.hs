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

parseCount :: [String] -> Either String Count
parseCount = foldM f empty . toAssoc
        where f a ("songs", x)    = parse parseNum
                                    (\x' -> a { cSongs = x'}) x
              f a ("playtime", x) = parse parseNum
                                    (\x' -> a { cPlaytime = x' }) x
              f _ x               = Left $ show x
              empty = Count { cSongs = 0, cPlaytime = 0 }

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

runParser :: ([String] -> Either String a) -> [String] -> MPD a
runParser f = either (throwError . Unexpected) return . f

-- A helper that runs a parser on a string and, depending, on the
-- outcome, either returns the result of some command applied to the
-- result, or fails. Used when building structures.
parse :: Monad m => (String -> Maybe a) -> (a -> b) -> String -> m b
parse p g x = maybe (fail x) (return . g) (p x)

-- A helper for running a parser returning Maybe on a pair of strings.
-- Returns Just if both strings where parsed successfully, Nothing otherwise.
pair :: (String -> Maybe a) -> (String, String) -> Maybe (a, a)
pair p (x, y) = case (p x, p y) of
                    (Just a, Just b) -> Just (a, b)
                    _                -> Nothing
