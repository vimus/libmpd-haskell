{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.CurrentPlaylist
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

The current playlist.
-}

module Network.MPD.Applicative.CurrentPlaylist
    ( add
    , addId
    , clear
    , delete
    , deleteRange
    , deleteId
    , move
    , moveId
    , moveRange
    , playlistFind
    , playlistInfo
    , playlistInfoRange
    , playlistId
    , playlistSearch
    , plChanges
    , plChangesPosId
    , shuffle
    , swap
    , swapId
    ) where

import           Network.MPD.Commands.Arg hiding (Command)
import qualified Network.MPD.Commands.Arg as Arg
import           Network.MPD.Util
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types
import           Network.MPD.Applicative.Internal
import           Network.MPD.Applicative.Util

-- | Add a song (or a whole directory) to the current playlist.
add :: Path -> Command ()
add path = Command emptyResponse ["add" <@> path]

-- | Add a song (non-recursively) and return its id.
addId :: Path -> Maybe Position -> Command Id
addId path pos = Command p c
  where
    c = ["addid" <@> path <++> pos]
    p = do
      r <- getResponse
      case toAssocList r of
        [("Id", n)] -> maybe (unexpected r) (return . Id) (parseNum n)
        _         -> unexpected r

-- | Clear the current playlist.
clear :: Command ()
clear = Command emptyResponse ["clear"]

-- | Delete song at the given playlist position.
delete :: Position -> Command ()
delete pos = Command emptyResponse ["delete" <@> pos]

-- XXX: this does not exist in the monadic version
-- | Delete a range of songs from the playlist.
deleteRange :: (Position, Position) -> Command ()
deleteRange range = Command emptyResponse ["delete" <@> range]

-- | Delete song by id.
deleteId :: Id -> Command ()
deleteId i = Command emptyResponse ["deleteid" <@> i]

-- | Move song from one position to another.
move :: Position -> Position -> Command ()
move pos to = Command emptyResponse ["move" <@> pos <++> to]

-- | Move a range of songs.
moveRange :: (Position, Position) -> Position -> Command ()
moveRange range to = Command emptyResponse ["move" <@> range <++> to]

-- | Move song id to position.
-- If the position is negative, it is relative to the current song.
moveId :: Id -> Position -> Command ()
moveId i to = Command emptyResponse ["moveid" <@> i <++> to]

-- Note: 'playlist' deliberately not defined here

-- Internal helper for playlist* commands
playlist' :: MPDArg a => Arg.Command -> a -> Command [Song]
playlist' cmd q = Command (liftParser takeSongs) [cmd <@> q]

-- | Find songs in current playlist with strict matching.
playlistFind :: Query -> Command [Song]
playlistFind = playlist' "playlistfind"

-- | Get song metadata for all items in the current playlist.
-- Optionally restrict listing the song at the given position.
playlistInfo :: Maybe Position -> Command [Song]
playlistInfo = playlist' "playlistinfo"

-- | Like 'playlistInfo' but can restrict listing to a range
-- of songs.
playlistInfoRange :: Maybe (Position, Position) -> Command [Song]
playlistInfoRange = playlist' "playlistinfo"

-- | Get song metadata for all items in the current playlist.
-- Optionally restrict selection to a single song id.
playlistId :: Maybe Id -> Command [Song]
playlistId = playlist' "playlistid"

-- | Search case-insensitively for partial matches in current playlist.
playlistSearch :: Query -> Command [Song]
playlistSearch = playlist' "playlistsearch"

-- | Get song metadata for items that have changed in the playlist since
-- the given playlist version.
plChanges :: Integer -> Command [Song]
plChanges = playlist' "plchanges"

-- | Get positions and ids of songs that have changed in the playlist
-- since the given playlist version.
plChangesPosId :: Integer -> Command [(Position, Id)]
plChangesPosId ver = Command p ["plchangesposid" <@> ver]
    where
        -- XXX: possibly suboptimal definition
        p :: Parser [(Position, Id)]
        p = liftParser $ mapM f . splitGroups ["cpos"] . toAssocList
        f xs | [("cpos", x), ("Id", y)] <- xs
             , Just (x', y') <- pair parseNum (x, y)
             = Right (x', Id y')
             | otherwise = Left ""

-- | Shuffle the current playlist.
-- Optionally restrict to a range of songs.
shuffle :: Maybe (Position, Position) -> Command ()
shuffle mbRange = Command emptyResponse ["shuffle" <@> mbRange]

-- | Swap songs by position.
swap :: Position -> Position -> Command ()
swap pos1 pos2 = Command emptyResponse ["swap" <@> pos1 <++> pos2]

-- | Swap songs by id.
swapId :: Id -> Id -> Command ()
swapId id1 id2 = Command emptyResponse ["swapid" <@> id1 <++> id2]
