{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Reflection
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Reflection.
-}

module Network.MPD.Applicative.Reflection
    ( commands
    , notCommands
    , tagTypes
    , urlHandlers
    , decoders
    ) where

import           Network.MPD.Util
import           Network.MPD.Applicative.Internal
import           Network.MPD.Applicative.Util

import           Control.Applicative
import           Prelude hiding (repeat, read)

import qualified Data.ByteString.UTF8 as UTF8

-- | Get a list of available commands.
commands :: Command [String]
commands = Command p ["commands"]
    where
        p = map UTF8.toString . takeValues <$> getResponse

-- | Get a list of unavailable commands (i.e., commands that require
-- an authenticated session).
notCommands :: Command [String]
notCommands = Command p ["notcommands"]
    where
        p = map UTF8.toString . takeValues <$> getResponse

-- | Get a list of available song metadata.
tagTypes :: Command [String]
tagTypes = Command p ["tagtypes"]
    where
        p = map UTF8.toString . takeValues <$> getResponse

-- | Get a list of available URL handlers.
urlHandlers :: Command [String]
urlHandlers = Command p ["urlhandlers"]
    where
        p = map UTF8.toString . takeValues <$> getResponse

-- | Get a list of available decoder plugins, with their supported
-- suffixes and MIME types.
decoders :: Command [(String, [(String, String)])]
decoders = Command p ["decoders"]
    where
        p = takeDecoders . toAssocList <$> getResponse

        takeDecoders [] = []
        takeDecoders ((_, m):xs) =
            let (info, rest) = break ((==) "plugin" . fst) xs
            in (UTF8.toString m, map decodePair info) : takeDecoders rest
