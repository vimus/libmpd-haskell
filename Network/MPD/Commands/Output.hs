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

import           Network.MPD.Core
import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util

-- | Turn off an output device.
disableOutput :: MonadMPD m => Int -> m ()
disableOutput = getResponse_ . ("disableoutput" <@>)

-- | Turn on an output device.
enableOutput :: MonadMPD m => Int -> m ()
enableOutput = getResponse_ . ("enableoutput" <@>)

-- | Retrieve information for all output devices.
outputs :: MonadMPD m => m [Device]
outputs = getResponse "outputs" >>= runParser parseOutputs
