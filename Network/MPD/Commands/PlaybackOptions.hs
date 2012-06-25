{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.PlaybackOptions
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

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

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util
import           Network.MPD.Core

import           Control.Monad (liftM)
import           Prelude hiding (repeat)

import qualified Data.ByteString.UTF8 as UTF8

-- | Set consume mode
consume :: MonadMPD m => Bool -> m ()
consume = getResponse_ . ("consume" <@>)

-- | Set crossfading between songs.
crossfade :: MonadMPD m => Seconds -> m ()
crossfade secs = getResponse_ ("crossfade" <@> secs)

-- | Set random playing.
random :: MonadMPD m => Bool -> m ()
random = getResponse_ . ("random" <@>)

-- | Set repeating.
repeat :: MonadMPD m => Bool -> m ()
repeat = getResponse_ . ("repeat" <@>)

-- | Set the volume (0-100 percent).
setVolume :: MonadMPD m => Int -> m ()
setVolume = getResponse_ . ("setvol" <@>)

-- | Set single mode
single :: MonadMPD m => Bool -> m ()
single = getResponse_ . ("single" <@>)

-- | Set the replay gain mode.
replayGainMode :: MonadMPD m => ReplayGainMode -> m ()
replayGainMode = getResponse_ . ("replay_gain_mode" <@>)

-- | Get the replay gain options.
replayGainStatus :: MonadMPD m => m [String]
replayGainStatus = map UTF8.toString `liftM` getResponse "replay_gain_status"
