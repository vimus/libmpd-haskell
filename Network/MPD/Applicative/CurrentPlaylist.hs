{-# LANGUAGE OverloadedStrings #-}
module Network.MPD.Applicative.CurrentPlaylist where

import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Util
import           Network.MPD.Commands.Types
import           Network.MPD.Applicative

-- | Add a song (or a whole directory) to the current playlist.
add :: Path -> Command ()
add path = Command emptyResponse ["add" <@> path]

addId :: Path -> Maybe Position -> Command Id
addId path pos = Command p c
  where
    c = ["addid" <@> path <++> pos]
    p = do
      r <- getResponse
      case toAssocList r of
        [("Id", n)] -> maybe (unexpected r) (return . Id) (parseNum n)
        _         -> unexpected r

clear :: Command ()
clear = Command emptyResponse ["clear"]

delete :: Position -> Command ()
delete pos = Command emptyResponse ["delete" <@> pos]

deleteRange :: (Position, Position) -> Command ()
deleteRange range = Command emptyResponse ["delete" <@> range]

deleteId :: Id -> Command ()
deleteId i = Command emptyResponse ["deleteid" <@> i]

move :: Position -> Position -> Command ()
move pos to = Command emptyResponse ["move" <@> pos <++> to]

moveRange :: (Position, Position) -> Position -> Command ()
moveRange range to = Command emptyResponse ["move" <@> range <++> to]

moveId :: Id -> Position -> Command ()
moveId i to = Command emptyResponse ["moveid" <@> i <++> to]
