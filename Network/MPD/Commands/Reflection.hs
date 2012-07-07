{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Reflection
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Reflection.
-}

module Network.MPD.Commands.Reflection
    ( commands
    , notCommands
    , tagTypes
    , urlHandlers
    , decoders
    ) where

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.Reflection as A
import           Network.MPD.Core

-- | Retrieve a list of available commands.
commands :: MonadMPD m => m [String]
commands = A.runCommand A.commands

-- | Retrieve a list of unavailable (due to access restrictions) commands.
notCommands :: MonadMPD m => m [String]
notCommands = A.runCommand A.notCommands

-- | Retrieve a list of available song metadata.
tagTypes :: MonadMPD m => m [String]
tagTypes = A.runCommand A.tagTypes

-- | Retrieve a list of supported urlhandlers.
urlHandlers :: MonadMPD m => m [String]
urlHandlers = A.runCommand A.urlHandlers

-- | Retreive a list of decoder plugins with associated suffix and mime types.
decoders :: MonadMPD m => m [(String, [(String, String)])]
decoders = A.runCommand A.decoders
