{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.PlaybackOptions
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : MIT (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Playback options.
-}

module Network.MPD.Commands.PlaybackOptions
    ( consume
    , crossfade
    , random
    , repeat
    , setVolume
    , single
    , replayGainMode
    , replayGainStatus
    ) where

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.PlaybackOptions as A
import           Network.MPD.Commands.Types
import           Network.MPD.Core

import           Prelude hiding (repeat)

-- | Set consume mode
consume :: MonadMPD m => Bool -> m ()
consume = A.runCommand . A.consume

-- | Set crossfading between songs.
crossfade :: MonadMPD m => Seconds -> m ()
crossfade = A.runCommand . A.crossfade

-- | Set random playing.
random :: MonadMPD m => Bool -> m ()
random = A.runCommand . A.random

-- | Set repeating.
repeat :: MonadMPD m => Bool -> m ()
repeat = A.runCommand . A.repeat

-- | Set the volume (0-100 percent).
setVolume :: MonadMPD m => Int -> m ()
setVolume = A.runCommand . A.setVolume

-- | Set single mode
single :: MonadMPD m => Bool -> m ()
single = A.runCommand . A.single

-- | Set the replay gain mode.
replayGainMode :: MonadMPD m => ReplayGainMode -> m ()
replayGainMode = A.runCommand . A.replayGainMode

-- | Get the replay gain options.
replayGainStatus :: MonadMPD m => m [(String, String)]
replayGainStatus = A.runCommand A.replayGainStatus
