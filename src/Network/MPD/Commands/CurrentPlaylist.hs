{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.CurrentPlaylist
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : MIT (see LICENSE)

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

The current playlist.
-}

module Network.MPD.Commands.CurrentPlaylist
    ( addId
    , add
    , clear
    , delete
    , deleteId
    , move
    , moveId
    , playlist
    , playlistFind
    , playlistInfo
    , playlistInfoRange
    , playlistId
    , playlistSearch
    , plChanges
    , plChangesPosId
    , prio
    , prioId
    , shuffle
    , swap
    , swapId
    ) where

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.CurrentPlaylist as A
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Types
import           Network.MPD.Core
import           Network.MPD.Util

import           Control.Monad.Error (throwError)

-- | Like 'add', but returns a playlist id.
addId :: MonadMPD m => Path -> Maybe Position -> m Id
addId path = A.runCommand . A.addId path

-- | Add a song (or a whole directory) to the current playlist.
add :: MonadMPD m => Path -> m ()
add = A.runCommand . A.add

-- | Clear the current playlist.
clear :: MonadMPD m => m ()
clear = A.runCommand A.clear

-- | Remove a song from the current playlist.
delete :: MonadMPD m => Position -> m ()
delete = A.runCommand . A.delete

-- | Remove a song from the current playlist.
deleteId :: MonadMPD m => Id -> m ()
deleteId = A.runCommand . A.deleteId

-- | Move a song to a given position in the current playlist.
move :: MonadMPD m => Position -> Position -> m ()
move pos = A.runCommand . A.move pos

-- | Move a song from (songid) to (playlist index) in the playlist. If to is
-- negative, it is relative to the current song in the playlist (if there is one).
moveId :: MonadMPD m => Id -> Position -> m ()
moveId i = A.runCommand . A.moveId i

-- | Retrieve file paths and positions of songs in the current playlist.
-- Note that this command is only included for completeness sake; it's
-- deprecated and likely to disappear at any time, please use 'playlistInfo'
-- instead.
playlist :: MonadMPD m => m [(Position, Path)]
playlist = mapM f =<< getResponse "playlist"
    where f s | (pos, name) <- breakChar ':' s
              , Just pos'   <- parseNum pos
              = return (pos', Path name)
              | otherwise = throwError . Unexpected $ show s
{-# WARNING playlist "this is deprecated; please use 'playlistInfo' instead." #-}

-- | Search for songs in the current playlist with strict matching.
playlistFind :: MonadMPD m => Query -> m [Song]
playlistFind = A.runCommand . A.playlistFind

-- | Retrieve metadata for songs in the current playlist.
playlistInfo :: MonadMPD m => Maybe Position -> m [Song]
playlistInfo = A.runCommand . A.playlistInfo

-- | Like 'playlistInfo' but can restrict to a range of songs.
playlistInfoRange :: MonadMPD m => Maybe (Position, Position) -> m [Song]
playlistInfoRange = A.runCommand . A.playlistInfoRange

-- | Displays a list of songs in the playlist.
-- If id is specified, only its info is returned.
playlistId :: MonadMPD m => Maybe Id -> m [Song]
playlistId = A.runCommand . A.playlistId

-- | Search case-insensitively with partial matches for songs in the
-- current playlist.
playlistSearch :: MonadMPD m => Query -> m [Song]
playlistSearch = A.runCommand . A.playlistSearch

-- | Retrieve a list of changed songs currently in the playlist since
-- a given playlist version.
plChanges :: MonadMPD m => Integer -> m [Song]
plChanges = A.runCommand . A.plChanges

-- | Like 'plChanges' but only returns positions and ids.
plChangesPosId :: MonadMPD m => Integer -> m [(Position, Id)]
plChangesPosId = A.runCommand . A.plChangesPosId

-- | Set the priority of the specified songs.
prio :: MonadMPD m => Priority -> (Position, Position) -> m ()
prio p = A.runCommand . A.prio p

-- | Set priority by song id.
prioId :: MonadMPD m => Priority -> Id -> m ()
prioId p = A.runCommand . A.prioId p

-- | Shuffle the playlist.
shuffle :: MonadMPD m => Maybe (Position, Position) -- ^ Optional range (start, end)
        -> m ()
shuffle = A.runCommand . A.shuffle

-- | Swap the positions of two songs.
swap :: MonadMPD m => Position -> Position -> m ()
swap pos1 = A.runCommand . A.swap pos1

-- | Swap the positions of two songs (Id version)
swapId :: MonadMPD m => Id -> Id -> m ()
swapId id1 = A.runCommand . A.swapId id1
