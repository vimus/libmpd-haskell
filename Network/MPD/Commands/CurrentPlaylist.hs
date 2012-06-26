{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.CurrentPlaylist
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
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
    , playlistId
    , playlistSearch
    , plChanges
    , plChangesPosId
    , shuffle
    , swap
    , swapId
    ) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util
import           Network.MPD.Core
import           Network.MPD.Util
import qualified Network.MPD.Applicative as A
import qualified Network.MPD.Applicative.CurrentPlaylist as A

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
delete pos = getResponse_ ("delete" <@> pos)

-- | Remove a song from the current playlist.
deleteId :: MonadMPD m => Id -> m ()
deleteId id' = getResponse_ ("deleteid" <@> id')

-- | Move a song to a given position in the current playlist.
move :: MonadMPD m => Position -> Position -> m ()
move pos to = getResponse_ ("move" <@> pos <++> to)

-- | Move a song from (songid) to (playlist index) in the playlist. If to is
-- negative, it is relative to the current song in the playlist (if there is one).
moveId :: MonadMPD m => Id -> Position -> m ()
moveId id' to = getResponse_ ("moveid" <@> id' <++> to)

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

-- | Search for songs in the current playlist with strict matching.
playlistFind :: MonadMPD m => Query -> m [Song]
playlistFind q = takeSongs =<< getResponse ("playlistfind" <@> q)

-- | Retrieve metadata for songs in the current playlist.
playlistInfo :: MonadMPD m => Maybe (Position, Position) -> m [Song]
playlistInfo range = takeSongs =<< getResponse ("playlistinfo" <@> range)

-- | Displays a list of songs in the playlist.
-- If id is specified, only its info is returned.
{-# WARNING playlistId "this function does not do what it looks like, and we will probably remove it!" #-}
playlistId :: MonadMPD m => Maybe Id -> m [Song]
-- FIXME: playlistinfo is used with an id here, but playlistinfo either takes a
-- range or a position, but never an id!
playlistId id' = takeSongs =<< getResponse ("playlistinfo" <@> id')

-- | Search case-insensitively with partial matches for songs in the
-- current playlist.
playlistSearch :: MonadMPD m => Query -> m [Song]
playlistSearch q = takeSongs =<< getResponse ("playlistsearch" <@> q)

-- | Retrieve a list of changed songs currently in the playlist since
-- a given playlist version.
plChanges :: MonadMPD m => Integer -> m [Song]
plChanges version = takeSongs =<< getResponse ("plchanges" <@> version)

-- | Like 'plChanges' but only returns positions and ids.
plChangesPosId :: MonadMPD m => Integer -> m [(Position, Id)]
plChangesPosId plver =
    getResponse ("plchangesposid" <@> plver) >>=
    mapM f . splitGroups ["cpos"] . toAssocList
    where f xs | [("cpos", x), ("Id", y)] <- xs
               , Just (x', y') <- pair parseNum (x, y)
               = return (x', Id y')
               | otherwise = throwError . Unexpected $ show xs

-- | Shuffle the playlist.
shuffle :: MonadMPD m => Maybe (Position, Position) -- ^ Optional range (start, end)
        -> m ()
shuffle range = getResponse_ ("shuffle" <@> range)

-- | Swap the positions of two songs.
swap :: MonadMPD m => Position -> Position -> m ()
swap pos1 pos2 = getResponse_ ("swap" <@> pos1 <++> pos2)

-- | Swap the positions of two songs (Id version)
swapId :: MonadMPD m => Id -> Id -> m ()
swapId id1 id2 = getResponse_ ("swapid" <@> id1 <++> id2)
