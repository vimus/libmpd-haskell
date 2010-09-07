{-# LANGUAGE FlexibleContexts #-}

-- | Module    : Network.MPD.Commands.Parse
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Parsers for MPD data types.

module Network.MPD.Commands.Parse (
    -- * Parsers for many objects
    parseSongs, parseEntries, parseOutputs,
    -- * Parsers for one object
    parseCount, parseStats, parseStatus,
    -- * Misc utilities
    parse, pair
    ) where

import Network.MPD.Commands.Types

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Arrow (first, (***))
import Control.Monad.Error
import Network.MPD.Utils
import Network.MPD.Core (MonadMPD, MPDError(Unexpected))

type ItemGen a = [(String, String)] -- ^ list of (key,value) pairs
               -> a                 -- ^ intermediate object
               -> (a, Int)          -- ^ result object along with information
                                    --   about amount of lines it consumed

-- | Generate Output object.
parseOutput :: ItemGen Output
parseOutput ls d = f ls d 0
    where f []                d n = (d, n)
          f ((key, value):ls) d n =
              case key of
                   "outputname"    -> f ls d { outName = value }      (succ n)
                   "outputenabled" -> f ls (up outenabled' parseBool) (succ n)
                   "outputid"      -> (d, n) -- new output, we're done here
                   _               -> f ls d (succ n) -- ignore unknown keys

              where up g parser = maybe d g $ parser value
                    outenabled' v = d { outEnabled = v }

-- | Generate Entry object
parseEntry :: ItemGen Entry
parseEntry ls (SongE s) = first SongE $ parseSong ls s
parseEntry _  d@(DirectoryE _) = (d, 0)
parseEntry ls (PlaylistE p) = first PlaylistE $ parsePlaylist ls p 0
    where parsePlaylist [] s n = (s, n)
          parsePlaylist ((key, value):ls) p n =
              case key of
                   "Last-Modified" -> parsePlaylist ls p
                                        { plLastModified = parseIso8601 value }
                                        (succ n)
                   "playlist"      -> (p, n) -- new playlist, we're done here
                   _               -> parsePlaylist ls p (succ n) -- ignore unknown keys

-- | Generate Song object
parseSong :: ItemGen Song
parseSong ls s = f ls s 0
    where f []                s n = (s, n)
          f ((key, value):ls) s n =
              case key of
                   "Last-Modified" -> f ls s { sgLastModified = lm   } (succ n)
                   "Time"          -> f ls s { sgLength       = time } (succ n)
                   "Id"            -> f ls s { sgIndex        = id'  } (succ n)
                   "Pos"           -> f ls s { sgIndex        = pos  } (succ n)
                   "file"          -> (s, n) -- new item, we're done here
                   "playlist"      -> (s, n) -- new item, we're done here
                   -- "directory" is not needed, files are always after dirs
                   _ -> case tagValue key of
                            Just meta -> f ls s
                                           { sgTags = M.insertWith' (++)
                                                 meta [value] (sgTags s)
                                           } (succ n)
                            Nothing   -> f ls s (succ n) -- ignore unknown keys

              where lm     = parseIso8601 value
                    time   = maybe 0 id $ parseNum value
                    id'    = Just (maybe 0 fst $ sgIndex s, maybe 0 id $ parseNum value)
                    pos    = Just (maybe 0 id $ parseNum value, maybe 0 snd $ sgIndex s)

                    -- Why not just derive instance of Read? Because
                    -- using read is a few times slower than this.
                    tagValue "Artist" = Just Artist
                    tagValue "ArtistSort" = Just ArtistSort
                    tagValue "Album" = Just Album
                    tagValue "AlbumArtist" = Just AlbumArtist
                    tagValue "AlbumArtistSort" = Just AlbumArtistSort
                    tagValue "Title" = Just Title
                    tagValue "Track" = Just Track
                    tagValue "Name" = Just Name
                    tagValue "Genre" = Just Genre
                    tagValue "Date" = Just Date
                    tagValue "Composer" = Just Composer
                    tagValue "Performer" = Just Performer
                    tagValue "Disc" = Just Disc
                    tagValue "MUSICBRAINZ_ARTISTID" = Just MUSICBRAINZ_ARTISTID
                    tagValue "MUSICBRAINZ_ALBUMID" = Just MUSICBRAINZ_ALBUMID
                    tagValue "MUSICBRAINZ_ALBUMARTISTID" = Just MUSICBRAINZ_ALBUMARTISTID
                    tagValue "MUSICBRAINZ_TRACKID" = Just MUSICBRAINZ_TRACKID
                    tagValue _ = Nothing

-------------------------------------------------------------------

-- | Parser generator.
genParser :: [(String, String -> a)] -> ItemGen a -> [(String, String)] -> [a]
genParser _    _      []                = []
genParser ptrs parser ((key, value):ls) =
    case key `lookup` ptrs of
        Just ctr -> let (item, n) = parser ls (ctr value) in
                        item : genParser ptrs parser (drop n ls)
        Nothing  ->            genParser ptrs parser ls

-- | Parser for songs.
parseSongs :: [(String, String)] -> [Song]
parseSongs = genParser [ ("file", initSong)
                       ] parseSong

initSong :: String -> Song
initSong filepath =
    Song { sgFilePath = filepath, sgTags = M.empty
         , sgLastModified = Nothing, sgLength = 0
         , sgIndex = Nothing }

-- | Parser for database entries.
parseEntries :: [(String, String)] -> [Entry]
parseEntries = genParser [ ("directory", DirectoryE)
                         , ("file", SongE . initSong)
                         , ("playlist", PlaylistE . initPlaylist)
                         ] parseEntry

initPlaylist :: String -> Playlist
initPlaylist name =
    Playlist { plName = name, plLastModified = Nothing }

-- | Parser for outputs.
parseOutputs :: [(String, String)] -> [Output]
parseOutputs = genParser [ ("outputid", initOutput . fromMaybe (-1) . parseNum )
                         ] parseOutput

initOutput :: Int -> Output
initOutput id' =
    Output { outID = id', outName = "", outEnabled = False }

------------------------------------------------------------------

-- | Builds a 'Count' instance from an assoc. list.
parseCount :: [(String, String)] -> Count
parseCount = flip parseCount' defaultCount

-- | Helper function.
parseCount' :: [(String, String)] -> Count -> Count
parseCount' []                c = c
parseCount' ((key, value):ls) c =
    case key of
        "songs"    -> modify parseNum $ \v -> c { cSongs = v }
        "playtime" -> modify parseNum $ \v -> c { cPlaytime = v }
        _          -> parseCount' ls c

    where modify p f = parseCount' ls $ parse p f c value

defaultCount =
    Count { cSongs = 0, cPlaytime = 0 }

-------------------------------------------------------------------

-- | Builds a 'Stats' instance from an assoc. list.
parseStats :: [(String, String)] -> Stats
parseStats = flip parseStats' defaultStats

-- | Helper function.
parseStats' :: [(String, String)] -> Stats -> Stats
parseStats' []                s = s
parseStats' ((key, value):ls) s =
    case key of
        "artists"     -> modify parseNum $ \v -> s { stsArtists = v }
        "albums"      -> modify parseNum $ \v -> s { stsAlbums = v }
        "songs"       -> modify parseNum $ \v -> s { stsSongs = v }
        "uptime"      -> modify parseNum $ \v -> s { stsUptime = v }
        "playtime"    -> modify parseNum $ \v -> s { stsPlaytime = v }
        "db_playtime" -> modify parseNum $ \v -> s { stsDbPlaytime = v }
        "db_update"   -> modify parseNum $ \v -> s { stsDbUpdate = v }
        _             -> parseStats' ls s

    where modify p f = parseStats' ls $ parse p f s value

defaultStats =
     Stats { stsArtists = 0, stsAlbums = 0, stsSongs = 0, stsUptime = 0
           , stsPlaytime = 0, stsDbPlaytime = 0, stsDbUpdate = 0 }

-------------------------------------------------------------------

-- | Builds a 'Status' instance from an assoc. list.
parseStatus :: [(String, String)] -> Status
parseStatus = flip parseStatus' defaultStatus

-- | Helper function.
parseStatus' :: [(String, String)] -> Status -> Status
parseStatus' []                s = s
parseStatus' ((key, value):ls) s =
    case key of
       "volume"         -> modify parseNum   $ \v -> s { stVolume = v }
       "repeat"         -> modify parseBool  $ \v -> s { stRepeat = v }
       "random"         -> modify parseBool  $ \v -> s { stRandom = v }
       "single"         -> modify parseBool  $ \v -> s { stSingle = v }
       "consume"        -> modify parseBool  $ \v -> s { stConsume = v }
       "playlist"       -> modify parseNum   $ \v -> s { stPlaylistID = v }
       "playlistlength" -> modify parseNum   $ \v -> s { stPlaylistLength = v }
       "xfade"          -> modify parseNum   $ \v -> s { stXFadeWidth = v }
       "mixrampdb"      -> modify parseFrac  $ \v -> s { stMixRampdB = v }
       "mixrampdelay"   -> modify parseFrac  $ \v -> s { stMixRampDelay = v }
       "state"          -> modify parseState $ \v -> s { stState = v }
       "song"           -> parseStatus' ls s { stSongPos = parseNum value }
       "songid"         -> parseStatus' ls s { stSongID = parseNum value }
       "time"           -> modify parseTime  $ \v -> s { stTime = v }
       "elapsed"        -> modify parseFrac  $ \v -> s { stTime = (v, snd $ stTime s) }
       "bitrate"        -> modify parseNum   $ \v -> s { stBitrate = v }
       "audio"          -> modify parseAudio $ \v -> s { stAudio = v }
       "nextsong"       -> parseStatus' ls s { stNextSongPos = parseNum value }
       "nextsongid"     -> parseStatus' ls s { stNextSongID = parseNum value }
       "error"          -> parseStatus' ls s { stError = Just value }
       _                -> parseStatus' ls s

    where modify p f = parseStatus' ls $ parse p f s value

          parseAudio = parseTriple ':' parseNum

          parseState "play"  = Just Playing
          parseState "pause" = Just Paused
          parseState "stop"  = Just Stopped
          parseState _       = Nothing

          parseTime s =
              case parseFrac *** parseNum $ breakChar ':' s of
                 (Just a, Just b) -> Just (a, b)
                 _                -> Nothing

defaultStatus =
    Status { stState = Stopped, stVolume = 0, stRepeat = False
           , stRandom = False, stPlaylistID = 0, stPlaylistLength = 0
           , stSongPos = Nothing, stSongID = Nothing, stTime = (0, 0)
           , stNextSongPos = Nothing, stNextSongID = Nothing
           , stBitrate = 0, stXFadeWidth = 0, stMixRampdB = 0
           , stMixRampDelay = 0, stAudio = (0, 0, 0), stUpdatingDb = 0
           , stSingle = False, stConsume = False, stError = Nothing }

-------------------------------------------------------------------

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
