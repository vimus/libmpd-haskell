-- | Unparsing for MPD objects

module Unparse (Unparse(..)) where

import qualified Data.Map as M
import           Network.MPD.Commands.Types
import           Network.MPD.Util

class Unparse parsed where
    unparse :: parsed -> String

instance Unparse Count where
    unparse x = unlines
                [ "songs: "    ++ show (cSongs x)
                , "playtime: " ++ show (cPlaytime x) 
                ]

instance Unparse Device where
    unparse x = unlines
        [ "outputid: "      ++ show (dOutputID x)
        , "outputname: "    ++ dOutputName x
        , "outputenabled: " ++ showBool (dOutputEnabled x)
        ]

instance Unparse Song where
    unparse s =
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
            toF' k v    = show k ++ ": " ++ toString v

instance Unparse Stats where
    unparse s = unlines
        [ "artists: " ++ show (stsArtists s)
        , "albums: " ++ show (stsAlbums s)
        , "songs: " ++ show (stsSongs s)
        , "uptime: " ++ show (stsUptime s)
        , "playtime: " ++ show (stsPlaytime s)
        , "db_playtime: " ++ show (stsDbPlaytime s)
        , "db_update: " ++ show (stsDbUpdate s)
        ]

instance Unparse Status where
    unparse s = unlines $
        [ "state: " ++ (case stState s of Playing -> "play"
                                          Paused  -> "pause"
                                          _       -> "stop")
        , "volume: " ++ show (stVolume s)
        , "repeat: " ++ showBool (stRepeat s)
        , "random: " ++ showBool (stRandom s)
        , "playlist: " ++ show (stPlaylistVersion s)
        , "playlistlength: " ++ show (stPlaylistLength s)
        , "xfade: " ++ show (stXFadeWidth s)
        , "xfade: " ++ show (stXFadeWidth s)
        , "audio: " ++ (let (x, y, z) = stAudio s in show x ++ ":" ++ show y ++
                                       ":" ++ show z)
        , "single: " ++ showBool (stSingle s)
        , "consume: " ++ showBool (stConsume s) 
        ]
        ++ maybe [] (\n -> ["updating_db: " ++ show n]) (stUpdatingDb s)
        ++ maybe [] (\n -> ["song: " ++ show n]) (stSongPos s)
        ++ maybe [] (\n -> ["songid: " ++ show n]) (stSongID s)
        ++ maybe [] (\n -> ["error: " ++ show n]) (stError s)
        ++ maybe [] (\(x, y) -> ["time: " ++ show x ++ ":" ++ show y]) (stTime  s)
        ++ maybe [] (\n -> ["bitrate: " ++ show n]) (stBitrate s)
