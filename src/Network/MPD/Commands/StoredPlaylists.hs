{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.StoredPlaylists
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : MIT (see LICENSE)

Maintainer  : joachifm@fastmail.fm
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

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.StoredPlaylists as A
import           Network.MPD.Commands.Types
import           Network.MPD.Core

-- | Retrieve a list of files in a given playlist.
listPlaylist :: MonadMPD m => PlaylistName -> m [Path]
listPlaylist = A.runCommand . A.listPlaylist

-- | Retrieve metadata for files in a given playlist.
listPlaylistInfo :: MonadMPD m => PlaylistName -> m [Song]
listPlaylistInfo = A.runCommand . A.listPlaylistInfo

-- | Retreive a list of stored playlists.
listPlaylists :: MonadMPD m => m [PlaylistName]
listPlaylists = A.runCommand A.listPlaylists

-- | Load an existing playlist.
load :: MonadMPD m => PlaylistName -> m ()
load = A.runCommand . A.load

-- | Add a song (or a whole directory) to a stored playlist.
-- Will create a new playlist if the one specified does not already exist.
playlistAdd :: MonadMPD m => PlaylistName -> Path -> m ()
playlistAdd plname = A.runCommand . A.playlistAdd plname

-- | Clear a playlist. If the specified playlist does not exist, it will be
-- created.
playlistClear :: MonadMPD m => PlaylistName -> m ()
playlistClear = A.runCommand . A.playlistClear

-- | Remove a song from a playlist.
playlistDelete :: MonadMPD m => PlaylistName -> Position -> m ()
playlistDelete name = A.runCommand . A.playlistDelete name

-- | Move a song to a given position in the playlist specified.
playlistMove :: MonadMPD m => PlaylistName -> Id -> Position -> m ()
playlistMove name from = A.runCommand . A.playlistMove name from

-- | Rename an existing playlist.
rename :: MonadMPD m
       => PlaylistName -- ^ Original playlist
       -> PlaylistName -- ^ New playlist name
       -> m ()
rename plname = A.runCommand . A.rename plname

-- | Delete existing playlist.
rm :: MonadMPD m => PlaylistName -> m ()
rm = A.runCommand . A.rm

-- | Save the current playlist.
save :: MonadMPD m => PlaylistName -> m ()
save = A.runCommand . A.save
