{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.StoredPlaylists
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Stored playlists.
-}

module Network.MPD.Applicative.StoredPlaylists
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

import           Network.MPD.Applicative.Internal
import           Network.MPD.Applicative.Util
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util (takeValues)
import           Network.MPD.Util

import           Control.Applicative

-- | List song items in the playlist.
listPlaylist :: PlaylistName -> Command [Path]
listPlaylist plName = Command p ["listplaylist" <@> plName]
    where
        p = map Path . takeValues <$> getResponse

-- | List song items in the playlist with metadata.
listPlaylistInfo :: PlaylistName -> Command [Song]
listPlaylistInfo plName =
    Command (liftParser takeSongs) ["listplaylistinfo" <@> plName]

-- | Get a list of stored playlists.
listPlaylists :: Command [PlaylistName]
listPlaylists = Command p ["listplaylists"]
    where
        p = map PlaylistName . go [] . toAssocList <$> getResponse

        -- XXX: need to fail gracefully here
        -- After each playlist name we get a timestamp
        go acc [] = acc
        go acc ((_, b):_:xs) = go (b : acc) xs
        go _ _ = error "listPlaylists: bug"

-- | Load playlist into the current queue.
load :: PlaylistName -> Command ()
load plName = Command emptyResponse ["load" <@> plName]

-- | Add a database path to the named playlist.
playlistAdd :: PlaylistName -> Path -> Command ()
playlistAdd plName path =
    Command emptyResponse ["playlistadd" <@> plName <++> path]

-- | Clear the playlist.
playlistClear :: PlaylistName -> Command ()
playlistClear plName = Command emptyResponse ["playlistclear" <@> plName]

-- | Delete the item at the given position from the playlist.
playlistDelete :: PlaylistName -> Position -> Command ()
playlistDelete name pos =
    Command emptyResponse ["playlistdelete" <@> name <++> pos]

-- | Move a song to a new position within the playlist.
playlistMove :: PlaylistName -> Id -> Position -> Command ()
playlistMove name from to =
    Command emptyResponse ["playlistmove" <@> name <++> from <++> to]

-- | Rename the playlist.
rename :: PlaylistName -> PlaylistName -> Command ()
rename plName new = Command emptyResponse ["rename" <@> plName <++> new]

-- | Remove the playlist.
rm :: PlaylistName -> Command ()
rm plName = Command emptyResponse ["rm" <@> plName]

-- | Save current queue to the named playlist.
save :: PlaylistName -> Command ()
save plName = Command emptyResponse ["save" <@> plName]
