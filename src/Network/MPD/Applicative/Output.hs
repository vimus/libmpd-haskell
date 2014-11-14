{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Output
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

Audio output devices.
-}

module Network.MPD.Applicative.Output
    ( disableOutput
    , enableOutput
    , toggleOutput
    , outputs
    ) where

import           Network.MPD.Applicative.Internal
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types

-- | Turn off output.
disableOutput :: Int -> Command ()
disableOutput n = Command emptyResponse ["disableoutput" <@> n]

-- | Turn on output.
enableOutput :: Int -> Command ()
enableOutput n = Command emptyResponse ["enableoutput" <@> n]

-- | Toggle output.
toggleOutput :: Int -> Command ()
toggleOutput n = Command emptyResponse ["toggleoutput" <@> n]

-- | Get information about all available output devices.
outputs :: Command [Device]
outputs = Command (liftParser parseOutputs) ["outputs"]
