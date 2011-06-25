{-# LANGUAGE FlexibleContexts #-}

-- | Module    : Network.MPD.Commands.Parse
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Parsers for MPD data types.

module Network.MPD.Commands.Parse where

import Network.MPD.Commands.Types

import Control.Arrow ((***))
import Control.Monad.Error
import Data.Maybe (fromMaybe)

import Network.MPD.Utils
import Network.MPD.Core (MonadMPD, MPDError(Unexpected))

-- | Builds a 'Count' instance from an assoc. list.
parseCount :: [String] -> Either String Count
parseCount = foldM f defaultCount . toAssocList
        where f :: Count -> (String, String) -> Either String Count
              f a ("songs", x)    = return $ parse parseNum
                                    (\x' -> a { cSongs = x'}) a x
              f a ("playtime", x) = return $ parse parseNum
                                    (\x' -> a { cPlaytime = x' }) a x
              f _ x               = Left $ show x

-- | Builds a list of 'Device' instances from an assoc. list
parseOutputs :: [String] -> Either String [Device]
parseOutputs = mapM (foldM f defaultDevice)
             . splitGroups [("outputid",id)]
             . toAssocList
    where f a ("outputid", x)      = return $ parse parseNum
                                     (\x' -> a { dOutputID = x' }) a x
          f a ("outputname", x)    = return a { dOutputName = x }
          f a ("outputenabled", x) = return $ parse parseBool
                                     (\x' -> a { dOutputEnabled = x'}) a x
          f _ x                    = fail $ show x

-- | Builds a 'Stats' instance from an assoc. list.
parseStats :: [String] -> Either String Stats
parseStats = foldM f defaultStats . toAssocList
    where
        f a ("artists", x)     = return $ parse parseNum
                                 (\x' -> a { stsArtists  = x' }) a x
        f a ("albums", x)      = return $ parse parseNum
                                 (\x' -> a { stsAlbums   = x' }) a x
        f a ("songs", x)       = return $ parse parseNum
                                 (\x' -> a { stsSongs    = x' }) a x
        f a ("uptime", x)      = return $ parse parseNum
                                 (\x' -> a { stsUptime   = x' }) a x
        f a ("playtime", x)    = return $ parse parseNum
                                 (\x' -> a { stsPlaytime = x' }) a x
        f a ("db_playtime", x) = return $ parse parseNum
                                 (\x' -> a { stsDbPlaytime = x' }) a x
        f a ("db_update", x)   = return $ parse parseNum
                                 (\x' -> a { stsDbUpdate = x' }) a x
        f _ x = fail $ show x

-- | Builds a 'Song' instance from an assoc. list.
parseSong :: [(String, String)] -> Either String Song
parseSong xs = foldM f defaultSong xs
    where
        f :: Song -> (String, String) -> Either String Song
        f s ("Last-Modified", v) =
            return s { sgLastModified = parseIso8601 v }
        f s ("Time", v) =
            return s { sgLength = fromMaybe 0 $ parseNum v }
        f s ("Id", v) =
            return $ parse parseNum (\v' -> s { sgId = Just $ Id v' }) s v
        f s ("Pos", v) =
            maybe (return $ parse parseNum
                                  (\v' -> s { sgIndex = Just v' }) s v)
                  (const $ return s)
                  (sgIndex s)
        f s ("file", v) =
            return s { sgFilePath = v }
        f s (k, v) = return . maybe s (\m -> sgAddTag m v s) $
                     readMeta k

        -- Custom-made Read instance
        readMeta "Artist" = Just Artist
        readMeta "Album" = Just Album
        readMeta "Title" = Just Title
        readMeta "Genre" = Just Genre
        readMeta "Name" = Just Name
        readMeta "Composer" = Just Composer
        readMeta "Performer" = Just Performer
        readMeta "Date" = Just Date
        readMeta "Track" = Just Track
        readMeta "Disc" = Just Disc
        readMeta "MUSICBRAINZ_ARTISTID" = Just MUSICBRAINZ_ARTISTID
        readMeta "MUSICBRAINZ_TRACKID" = Just MUSICBRAINZ_TRACKID
        readMeta _ = Nothing

-- | Builds a 'Status' instance from an assoc. list.
parseStatus :: [String] -> Either String Status
parseStatus = foldM f defaultStatus . toAssocList
    where f a ("state", x)
              = return $ parse state     (\x' -> a { stState = x' }) a x
          f a ("volume", x)
              = return $ parse parseNum  (\x' -> a { stVolume = x' }) a x
          f a ("repeat", x)
              = return $ parse parseBool (\x' -> a { stRepeat = x' }) a x
          f a ("random", x)
              = return $ parse parseBool (\x' -> a { stRandom = x' }) a x
          f a ("playlist", x)
              = return $ parse parseNum  (\x' -> a { stPlaylistVersion = x' }) a x
          f a ("playlistlength", x)
              = return $ parse parseNum  (\x' -> a { stPlaylistLength = x' }) a x
          f a ("xfade", x)
              = return $ parse parseNum  (\x' -> a { stXFadeWidth = x' }) a x
          f a ("mixrampdb", x)
              = return $ parse parseFrac (\x' -> a { stMixRampdB = x' }) a x
          f a ("mixrampdelay", x)
              = return $ parse parseFrac (\x' -> a { stMixRampDelay = x' }) a x
          f a ("song", x)
              = return $ parse parseNum  (\x' -> a { stSongPos = Just x' }) a x
          f a ("songid", x)
              = return $ parse parseNum  (\x' -> a { stSongID = Just x' }) a x
          f a ("time", x)
              = return $ parse time      (\x' -> a { stTime = x' }) a x
          f a ("elapsed", x)
              = return $ parse parseFrac (\x' -> a { stTime = (x', snd $ stTime a) }) a x
          f a ("bitrate", x)
              = return $ parse parseNum  (\x' -> a { stBitrate = x' }) a x
          f a ("audio", x)
              = return $ parse audio     (\x' -> a { stAudio = x' }) a x
          f a ("updating_db", x)
              = return $ parse parseNum  (\x' -> a { stUpdatingDb = x' }) a x
          f a ("error", x)
              = return a { stError = Just x }
          f a ("single", x)
              = return $ parse parseBool (\x' -> a { stSingle = x' }) a x
          f a ("consume", x)
              = return $ parse parseBool (\x' -> a { stConsume = x' }) a x
          f a ("nextsong", x)
              = return $ parse parseNum  (\x' -> a { stNextSongPos = Just x' }) a x
          f a ("nextsongid", x)
              = return $ parse parseNum  (\x' -> a { stNextSongID = Just x' }) a x
          f _ x
              = fail $ show x

          state "play"  = Just Playing
          state "pause" = Just Paused
          state "stop"  = Just Stopped
          state _       = Nothing

          time s = case parseFrac *** parseNum $ breakChar ':' s of
                       (Just a, Just b) -> Just (a, b)
                       _                -> Nothing

          audio = parseTriple ':' parseNum

-- | Run a parser and lift the result into the 'MPD' monad
runParser :: (MonadMPD m, MonadError MPDError m)
          => (input -> Either String a) -> input -> m a
runParser f = either (throwError . Unexpected) return . f

-- | A helper that runs a parser on a string and, depending on the
-- outcome, either returns the result of some command applied to the
-- result, or a default value. Used when building structures.
parse :: (String -> Maybe a) -> (a -> b) -> b -> String -> b
parse parser f x = maybe x f . parser

-- | A helper for running a parser returning Maybe on a pair of strings.
-- Returns Just if both strings where parsed successfully, Nothing otherwise.
pair :: (String -> Maybe a) -> (String, String) -> Maybe (a, a)
pair p (x, y) = case (p x, p y) of
                    (Just a, Just b) -> Just (a, b)
                    _                -> Nothing
