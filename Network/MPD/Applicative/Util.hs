{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.Util where

import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types
import           Network.MPD.Util

import           Control.Monad (liftM)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

-- Separate the result of an lsinfo\/listallinfo call into directories,
-- playlists, and songs.
takeEntries :: [ByteString] -> Either String [LsResult]
takeEntries = mapM toEntry . splitGroups groupHeads . toAssocList
    where
        toEntry xs@(("file",_):_)   = LsSong `liftM` parseSong xs
        toEntry (("directory",d):_) = (return . LsDirectory . Path) d
        toEntry (("playlist",pl):_) = (return . LsPlaylist . PlaylistName) pl
        toEntry _ = error "takeEntries: splitGroups is broken"
        groupHeads = ["file", "directory", "playlist"]

takeSongs :: [ByteString] -> Either String [Song]
takeSongs = mapM parseSong . splitGroups ["file"] . toAssocList

-- Run 'toAssocList' and return only the values.
takeValues :: [ByteString] -> [ByteString]
takeValues = snd . unzip . toAssocList

-- an internal helper function
decodePair :: (ByteString, ByteString) -> (String, String)
decodePair (x, y) = (UTF8.toString x, UTF8.toString y)
