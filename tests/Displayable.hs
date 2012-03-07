{-# OPTIONS_GHC -Wwarn #-}

module Displayable (Displayable(..)) where

import qualified Data.Map as M
import           Network.MPD.Commands.Types
import           Network.MPD.Util

-- | A uniform interface for types that
-- can be turned into raw responses
class Displayable a where
    empty   :: a             -- ^ An empty instance
    display :: a -> String   -- ^ Transform instantiated object to a
                             --   string

instance Displayable Count where
    empty = defaultCount
    display s = unlines
        ["songs: "    ++ show (cSongs s)
        ,"playtime: " ++ show (cPlaytime s)]

instance Displayable Device where
    empty = defaultDevice
    display d = unlines
        ["outputid: "      ++ show (dOutputID d)
        ,"outputname: "    ++ dOutputName d
        ,"outputenabled: " ++ showBool (dOutputEnabled d)]

instance Displayable Song where
    empty = error "There is no notion of an empty song.  A song always has at least a file path."
    display s =
        let fs  = concatMap toF . M.toList $ sgTags s
            id_ = maybe [] (\(Id n) -> ["Id: " ++ show n]) (sgId s)
            idx = maybe [] (\n -> ["Pos: " ++ show n]) (sgIndex s)
            lastModified = maybe [] (return . ("Last-Modified: " ++) . formatIso8601) (sgLastModified s)
        in unlines $ ["file: " ++ (toString . sgFilePath) s]
                  ++ ["Time: " ++ (show . sgLength) s]
                  ++ fs
                  ++ lastModified
                  ++ id_
                  ++ idx
        where
            toF (k, vs) = map (toF' k) vs
            toF' k v    = show k ++ ": " ++ v

instance Displayable Stats where
    empty = defaultStats
    display s = unlines
        ["artists: " ++ show (stsArtists s)
        ,"albums: " ++ show (stsAlbums s)
        ,"songs: " ++ show (stsSongs s)
        ,"uptime: " ++ show (stsUptime s)
        ,"playtime: " ++ show (stsPlaytime s)
        ,"db_playtime: " ++ show (stsDbPlaytime s)
        ,"db_update: " ++ show (stsDbUpdate s)]

instance Displayable Status where
    empty = defaultStatus
    display s = unlines $
        ["state: " ++ (case stState s of Playing -> "play"
                                         Paused  -> "pause"
                                         _       -> "stop")
        ,"volume: " ++ show (stVolume s)
        ,"repeat: " ++ showBool (stRepeat s)
        ,"random: " ++ showBool (stRandom s)
        ,"playlist: " ++ show (stPlaylistVersion s)
        ,"playlistlength: " ++ show (stPlaylistLength s)
        ,"xfade: " ++ show (stXFadeWidth s)
        ,"time: " ++ (let (x, y) = stTime s in show x ++ ":" ++ show y)
        ,"bitrate: " ++ show (stBitrate s)
        ,"xfade: " ++ show (stXFadeWidth s)

        ,"audio: " ++ (let (x, y, z) = stAudio s in show x ++ ":" ++ show y ++
                                       ":" ++ show z)
        ,"updating_db: " ++ show (stUpdatingDb s)
        ,"error: " ++ show (stError s)
        ,"single: " ++ showBool (stSingle s)
        ,"consume: " ++ showBool (stConsume s)]
        ++ maybe [] (\n -> ["song: " ++ show n]) (stSongPos s)
        ++ maybe [] (\n -> ["songid: " ++ show n]) (stSongID s)
