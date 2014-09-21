{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.PlaybackOptions
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachifm@fastmail.fm
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
    , mixrampDb
    , mixrampDelay
    ) where

import           Network.MPD.Applicative.Internal
import           Network.MPD.Applicative.Util
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Types
import           Network.MPD.Util (toAssocList)

import           Control.Applicative
import           Prelude hiding (repeat)

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

-- | Get replay gain status: option name and its value.
replayGainStatus :: Command [(String, String)]
replayGainStatus = Command p ["replay_gain_status"]
    where
        p = map decodePair . toAssocList <$> getResponse

-- | Set MixRamp overlap threshold.
-- 0dB is the normalized maximum value; use negative values to adjust it.
--
-- Songs must have MixRamp tags set by an external tool for this to
-- work; crossfading is used if no tags are present.
mixrampDb :: Decibels -> Command ()
mixrampDb db = Command emptyResponse ["mixrampdb" <@> db]

-- | Additional time subtracted from the overlap calculated by
-- 'mixrampDb'.
-- "NaN" disables MixRamp overlapping and reverts to crossfading.
mixrampDelay :: Seconds -> Command ()
mixrampDelay sec = Command emptyResponse ["mixrampdelay" <@> sec]
