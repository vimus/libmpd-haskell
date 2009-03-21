{-# LANGUAGE FlexibleContexts #-}

-- | Module    : Network.MPD.Commands.Parse
-- Copyright   : (c) Ben Sinclair 2005-2009
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
--
-- Various data types and parsing functions for them.

module Network.MPD.Commands.Parse where

import Network.MPD.Commands.Types

import Control.Monad.Error
import Network.MPD.Utils
import Network.MPD.Core (MonadMPD, MPDError(Unexpected))

-- | Builds a 'Count' instance from an assoc. list.
parseCount :: [String] -> Either String Count
parseCount = foldM f empty . toAssoc
        where f :: Count -> (String, String) -> Either String Count
              f a ("songs", x)    = return $ parse parseNum
                                    (\x' -> a { cSongs = x'}) a x
              f a ("playtime", x) = return $ parse parseNum
                                    (\x' -> a { cPlaytime = x' }) a x
              f _ x               = Left $ show x
              empty = Count { cSongs = 0, cPlaytime = 0 }

-- | Builds a list of 'Device' instances from an assoc. list
parseOutputs :: [String] -> Either String [Device]
parseOutputs = mapM (foldM f empty) . splitGroups [("outputid",id)] . toAssoc
    where f a ("outputid", x)      = return $ parse parseNum
                                     (\x' -> a { dOutputID = x' }) a x
          f a ("outputname", x)    = return a { dOutputName = x }
          f a ("outputenabled", x) = return $ parse parseBool
                                     (\x' -> a { dOutputEnabled = x'}) a x
          f _ x                    = fail $ show x
          empty = Device 0 "" False

-- | Builds a 'Stats' instance from an assoc. list.
parseStats :: [String] -> Either String Stats
parseStats = foldM f defaultStats . toAssoc
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
          f a ("Date", x)      = return $ parse parseDate
                                 (\x' -> a { sgDate = x' }) a x
          f a ("Track", x)     = return $ parse parseTuple
                                 (\x' -> a { sgTrack = x'}) a x
          f a ("Disc", x)      = return a { sgDisc = parseTuple x }
          f a ("file", x)      = return a { sgFilePath = x }
          f a ("Time", x)      = return $ parse parseNum
                                 (\x' -> a { sgLength = x'}) a x
          f a ("Id", x)        = return $ parse parseNum
                                 (\x' -> a { sgIndex = Just (ID x') }) a x
          -- We prefer Id but take Pos if no Id has been found.
          f a ("Pos", x)       =
              maybe (return $ parse parseNum
                           (\x' -> a { sgIndex = Just (Pos x') }) a x)
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
                      , sgDisc = Nothing, sgFilePath = "", sgLength = 0
                      , sgIndex = Nothing }

-- | Builds a 'Status' instance from an assoc. list.
parseStatus :: [String] -> Either String Status
parseStatus = foldM f empty . toAssoc
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
          f a ("song", x)
              = return $ parse parseNum  (\x' -> a { stSongPos = Just (Pos x') }) a x
          f a ("songid", x)
              = return $ parse parseNum  (\x' -> a { stSongID = Just (ID x') }) a x
          f a ("time", x)
              = return $ parse time      (\x' -> a { stTime = x' }) a x
          f a ("bitrate", x)
              = return $ parse parseNum  (\x' -> a { stBitrate = x' }) a x
          f a ("audio", x)
              = return $ parse audio     (\x' -> a { stAudio = x' }) a x
          f a ("updating_db", x)
              = return $ parse parseNum  (\x' -> a { stUpdatingDb = x' }) a x
          f a ("error", x)
              = return a { stError = x }
          f _ x
              = fail $ show x

          state "play"  = Just Playing
          state "pause" = Just Paused
          state "stop"  = Just Stopped
          state _       = Nothing

          time = pair parseNum . breakChar ':'

          audio s = let (u, u') = breakChar ':' s
                        (v, w)  = breakChar ':' u' in
                    case (parseNum u, parseNum v, parseNum w) of
                        (Just a, Just b, Just c) -> Just (a, b, c)
                        _                        -> Nothing

          empty = Status Stopped 0 False False 0 0 Nothing Nothing (0,0) 0 0
                  (0,0,0) 0 ""

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
