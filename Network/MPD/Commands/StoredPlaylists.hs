{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.StoredPlaylists
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Stored playlists.
-}

module Network.MPD.Commands.StoredPlaylists
    ( listPlaylist
    , listPlaylistInfo
    , listPlaylists
    , load
    , playlistAdd
    , playlistClear
    , playlistDelete
    , playlistMove
    , rename
    , rm
    , save
    ) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util
import           Network.MPD.Core
import           Network.MPD.Util

import           Control.Monad (liftM)

-- | Retrieve a list of files in a given playlist.
listPlaylist :: MonadMPD m => PlaylistName -> m [Path]
listPlaylist plname =
    (map Path . takeValues) `liftM` getResponse ("listplaylist" <@> plname)

-- | Retrieve metadata for files in a given playlist.
listPlaylistInfo :: MonadMPD m => PlaylistName -> m [Song]
listPlaylistInfo plname =
    takeSongs =<< getResponse ("listplaylistinfo" <@> plname)

-- | Retreive a list of stored playlists.
listPlaylists :: MonadMPD m => m [PlaylistName]
listPlaylists = (map PlaylistName . go [] . toAssocList) `liftM` getResponse "listplaylists"
    where
        -- After each playlist name we get a timestamp
        go acc [] = acc
        go acc ((_, b):_:xs) = go (b : acc) xs
        go _ _ = error "listPlaylists: bug"

-- | Load an existing playlist.
load :: MonadMPD m => PlaylistName -> m ()
load plname = getResponse_ ("load" <@> plname)

-- | Add a song (or a whole directory) to a stored playlist.
-- Will create a new playlist if the one specified does not already exist.
playlistAdd :: MonadMPD m => PlaylistName -> Path -> m ()
playlistAdd plname path = getResponse_ ("playlistadd" <@> plname <++> path)

-- | Clear a playlist. If the specified playlist does not exist, it will be
-- created.
playlistClear :: MonadMPD m => PlaylistName -> m ()
playlistClear = getResponse_ . ("playlistclear" <@>)

-- | Remove a song from a playlist.
playlistDelete :: MonadMPD m => PlaylistName
               -> Integer -- ^ Playlist position
               -> m ()
playlistDelete name pos = getResponse_ ("playlistdelete" <@> name <++> pos)

-- | Move a song to a given position in the playlist specified.
playlistMove :: MonadMPD m => PlaylistName -> Integer -> Integer -> m ()
playlistMove name from to =
    getResponse_ ("playlistmove" <@> name <++> from <++> to)

-- | Rename an existing playlist.
rename :: MonadMPD m
       => PlaylistName -- ^ Original playlist
       -> PlaylistName -- ^ New playlist name
       -> m ()
rename plname new = getResponse_ ("rename" <@> plname <++> new)

-- | Delete existing playlist.
rm :: MonadMPD m => PlaylistName -> m ()
rm plname = getResponse_ ("rm" <@> plname)

-- | Save the current playlist.
save :: MonadMPD m => PlaylistName -> m ()
save plname = getResponse_ ("save" <@> plname)
