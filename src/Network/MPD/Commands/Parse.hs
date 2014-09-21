{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- | Module    : Network.MPD.Commands.Parse
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : MIT (see LICENSE)
-- Maintainer  : Joachim Fasting <joachifm@fastmail.fm>
-- Stability   : alpha
--
-- Parsers for MPD data types.

module Network.MPD.Commands.Parse where

import           Network.MPD.Commands.Types

import           Control.Applicative
import           Control.Monad.Error
import           Data.Maybe (fromMaybe)

import           Network.MPD.Util

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

-- | Builds a 'Count' instance from an assoc. list.
parseCount :: [ByteString] -> Either String Count
parseCount = foldM f def . toAssocList
        where f :: Count -> (ByteString, ByteString) -> Either String Count
              f a ("songs", x)    = return $ parse parseNum
                                    (\x' -> a { cSongs = x'}) a x
              f a ("playtime", x) = return $ parse parseNum
                                    (\x' -> a { cPlaytime = x' }) a x
              f _ x               = Left $ show x

-- | Builds a list of 'Device' instances from an assoc. list
parseOutputs :: [ByteString] -> Either String [Device]
parseOutputs = mapM (foldM f def)
             . splitGroups ["outputid"]
             . toAssocList
    where f a ("outputid", x)      = return $ parse parseNum
                                     (\x' -> a { dOutputID = x' }) a x
          f a ("outputname", x)    = return a { dOutputName = UTF8.toString x }
          f a ("outputenabled", x) = return $ parse parseBool
                                     (\x' -> a { dOutputEnabled = x'}) a x
          f _ x                    = Left $ show x

-- | Builds a 'Stats' instance from an assoc. list.
parseStats :: [ByteString] -> Either String Stats
parseStats = foldM f def . toAssocList
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
        f _ x = Left $ show x

parseMaybeSong :: [ByteString] -> Either String (Maybe Song)
parseMaybeSong xs | null xs   = Right Nothing
                  | otherwise = Just <$> (parseSong . toAssocList) xs

-- | Builds a 'Song' instance from an assoc. list.
parseSong :: [(ByteString, ByteString)] -> Either String Song
parseSong xs = case xs of
    ("file", path):ys -> foldM f (defaultSong (Path path)) ys
    _ -> Left "Got a song without a file path! This indicates a bug in either libmpd-haskell or MPD itself!"

    where
        f :: Song -> (ByteString, ByteString) -> Either String Song
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
        f s (k, v) = return . maybe s (\m -> sgAddTag m (Value v) s) $
                     readMeta k

        -- Custom-made Read instance
        readMeta "ArtistSort" = Just ArtistSort
        readMeta "Artist" = Just Artist
        readMeta "Album" = Just Album
        readMeta "AlbumArtist" = Just AlbumArtist
        readMeta "AlbumArtistSort" = Just AlbumArtistSort
        readMeta "Title" = Just Title
        readMeta "Genre" = Just Genre
        readMeta "Name" = Just Name
        readMeta "Composer" = Just Composer
        readMeta "Performer" = Just Performer
        readMeta "Comment" = Just Comment
        readMeta "Date" = Just Date
        readMeta "Track" = Just Track
        readMeta "Disc" = Just Disc
        readMeta "MUSICBRAINZ_ARTISTID" = Just MUSICBRAINZ_ARTISTID
        readMeta "MUSICBRAINZ_ALBUMID" = Just MUSICBRAINZ_ALBUMID
        readMeta "MUSICBRAINZ_ALBUMARTISTID" = Just MUSICBRAINZ_ALBUMARTISTID
        readMeta "MUSICBRAINZ_TRACKID" = Just MUSICBRAINZ_TRACKID
        readMeta _ = Nothing

-- | A helper that runs a parser on a string and, depending on the
-- outcome, either returns the result of some command applied to the
-- result, or a default value. Used when building structures.
parse :: (ByteString -> Maybe a) -> (a -> b) -> b -> ByteString -> b
parse parser f x = maybe x f . parser

-- | A helper for running a parser returning Maybe on a pair of strings.
-- Returns Just if both strings where parsed successfully, Nothing otherwise.
pair :: (ByteString -> Maybe a) -> (ByteString, ByteString) -> Maybe (a, a)
pair p (x, y) = case (p x, p y) of
                    (Just a, Just b) -> Just (a, b)
                    _                -> Nothing
