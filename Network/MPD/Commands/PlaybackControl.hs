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

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.PlaybackControl as A
import           Network.MPD.Commands.Types
import           Network.MPD.Core

-- | Play the next song.
next :: MonadMPD m => m ()
next = A.runCommand A.next

-- | Pause playing.
pause :: MonadMPD m => Bool -> m ()
pause = A.runCommand . A.pause

-- | Begin\/continue playing.
play :: MonadMPD m => Maybe Position -> m ()
play = A.runCommand . A.play

-- | Play a file with given id.
playId :: MonadMPD m => Id -> m ()
playId = A.runCommand . A.playId

-- | Play the previous song.
previous :: MonadMPD m => m ()
previous = A.runCommand A.previous

-- | Seek to some point in a song.
seek :: MonadMPD m => Position -> Seconds -> m ()
seek pos = A.runCommand . A.seek pos

-- | Seek to some point in a song (id version)
seekId :: MonadMPD m => Id -> Seconds -> m ()
seekId id' = A.runCommand . A.seekId id'

-- | Stop playing.
stop :: MonadMPD m => m ()
stop = A.runCommand A.stop
