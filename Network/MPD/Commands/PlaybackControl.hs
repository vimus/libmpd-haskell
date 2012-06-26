{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.PlaybackControl
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Controlling playback.
-}

module Network.MPD.Commands.PlaybackControl
    ( next
    , pause
    , play
    , playId
    , previous
    , seek
    , seekId
    , stop
    ) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util
import           Network.MPD.Core

-- | Play the next song.
next :: MonadMPD m => m ()
next = getResponse_ "next"

-- | Pause playing.
pause :: MonadMPD m => Bool -> m ()
pause = getResponse_ . ("pause" <@>)

-- | Begin\/continue playing.
play :: MonadMPD m => Maybe Position -> m ()
play (Just pos) = getResponse_ ("play" <@> pos)
play _          = getResponse_  "play"

-- | Play a file with given id.
playId :: MonadMPD m => Id -> m ()
playId id' = getResponse_ ("playid" <@> id')

-- | Play the previous song.
previous :: MonadMPD m => m ()
previous = getResponse_ "previous"

-- | Seek to some point in a song.
seek :: MonadMPD m => Position -> Seconds -> m ()
seek pos time = getResponse_ ("seek" <@> pos <++> time)

-- | Seek to some point in a song (id version)
seekId :: MonadMPD m => Id -> Seconds -> m ()
seekId id' time = getResponse_ ("seekid" <@> id' <++> time)

-- | Stop playing.
stop :: MonadMPD m => m ()
stop = getResponse_ "stop"
