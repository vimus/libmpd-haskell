-- | Module    : Network.MPD.Parse
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- Various data types and parsing functions for them.

module Network.MPD.Parse where

import Network.MPD.Types

import Control.Monad.Error
import Network.MPD.Utils
import Network.MPD.Core (MPD, MPDError(Unexpected))

-- | Builds a 'Count' instance from an assoc. list.
parseCount :: [String] -> Either String Count
parseCount = foldM f empty . toAssoc
        where f a ("songs", x)    = parse parseNum
                                    (\x' -> a { cSongs = x'}) x
              f a ("playtime", x) = parse parseNum
                                    (\x' -> a { cPlaytime = x' }) x
              f _ x               = Left $ show x
              empty = Count { cSongs = 0, cPlaytime = 0 }

-- | Builds a list of 'Device' instances from an assoc. list
parseOutputs :: [String] -> Either String [Device]
parseOutputs = mapM (foldM f empty) . splitGroups [("outputid",id)] . toAssoc
    where f a ("outputid", x)      = parse parseNum (\x' -> a { dOutputID = x' }) x
          f a ("outputname", x)    = return a { dOutputName = x }
          f a ("outputenabled", x) = parse parseBool
                                     (\x' -> a { dOutputEnabled = x'}) x
          f _ x                    = fail $ show x
          empty = Device 0 "" False

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
