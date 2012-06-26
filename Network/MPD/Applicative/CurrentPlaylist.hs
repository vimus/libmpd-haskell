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
