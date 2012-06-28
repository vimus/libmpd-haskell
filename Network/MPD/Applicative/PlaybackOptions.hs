{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.PlaybackOptions
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Playback options
-}

module Network.MPD.Applicative.PlaybackOptions
    ( consume
    , crossfade
    , random
    , repeat
    , setVolume
    , single
    , replayGainMode
    , replayGainStatus
    ) where

import           Network.MPD.Applicative
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Types

import           Control.Applicative
import           Prelude hiding (repeat)

import qualified Data.ByteString.UTF8 as UTF8

-- | Toggle consume mode.
consume :: Bool -> Command ()
consume f = Command emptyResponse ["consume" <@> f]

-- | Set crossfading between songs.
crossfade :: Seconds -> Command ()
crossfade secs = Command emptyResponse ["crossfade" <@> secs]

-- | Toggle random mode.
random :: Bool -> Command ()
random f = Command emptyResponse ["random" <@> f]

-- | Toggle repeat mode.
repeat :: Bool -> Command ()
repeat f = Command emptyResponse ["repeat" <@> f]

-- | Set volume in percent.
setVolume :: Int -> Command ()
setVolume vol = Command emptyResponse ["setvol" <@> vol]

-- | Toggle single mode.
single :: Bool -> Command ()
single f = Command emptyResponse ["single" <@> f]

-- | Set replay gain mode.
replayGainMode :: ReplayGainMode -> Command ()
replayGainMode f = Command emptyResponse ["replay_gain_mode" <@> f]

-- | Get replay gain status.
replayGainStatus :: Command [String]
replayGainStatus = Command p ["replay_gain_status"]
    where
        p = map UTF8.toString <$> getResponse
