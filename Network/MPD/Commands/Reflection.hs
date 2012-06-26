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

import           Network.MPD.Commands.Util
import           Network.MPD.Core
import           Network.MPD.Util

import           Control.Monad (liftM)
import           Prelude hiding (repeat, read)

import qualified Data.ByteString.UTF8 as UTF8

-- | Retrieve a list of available commands.
commands :: MonadMPD m => m [String]
commands = (map UTF8.toString . takeValues) `liftM` getResponse "commands"

-- | Retrieve a list of unavailable (due to access restrictions) commands.
notCommands :: MonadMPD m => m [String]
notCommands = (map UTF8.toString . takeValues) `liftM` getResponse "notcommands"

-- | Retrieve a list of available song metadata.
tagTypes :: MonadMPD m => m [String]
tagTypes = (map UTF8.toString . takeValues) `liftM` (getResponse "tagtypes")

-- | Retrieve a list of supported urlhandlers.
urlHandlers :: MonadMPD m => m [String]
urlHandlers = (map UTF8.toString . takeValues) `liftM` (getResponse "urlhandlers")

-- | Retreive a list of decoder plugins with associated suffix and mime types.
decoders :: MonadMPD m => m [(String, [(String, String)])]
decoders = (takeDecoders . toAssocList) `liftM` getResponse "decoders"
    where
        takeDecoders [] = []
        takeDecoders ((_, p):xs) =
            let (info, rest) = break ((==) "plugin" . fst) xs
            in (UTF8.toString p, map decodePair info) : takeDecoders rest
