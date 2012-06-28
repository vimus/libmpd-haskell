{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Output
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Audio output devices.
-}

module Network.MPD.Commands.Output
    ( disableOutput
    , enableOutput
    , outputs
    ) where

import qualified Network.MPD.Applicative as A
import qualified Network.MPD.Applicative.Output as A
import           Network.MPD.Core
import           Network.MPD.Commands.Types

-- | Turn off an output device.
disableOutput :: MonadMPD m => Int -> m ()
disableOutput = A.runCommand . A.disableOutput

-- | Turn on an output device.
enableOutput :: MonadMPD m => Int -> m ()
enableOutput = A.runCommand . A.enableOutput

-- | Retrieve information for all output devices.
outputs :: MonadMPD m => m [Device]
outputs = A.runCommand A.outputs
