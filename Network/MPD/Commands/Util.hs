-- | Module    : Network.MPD.Commands.Util
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Internal utilities for implementing MPD commands.

module Network.MPD.Commands.Util where

import Network.MPD.Commands.Parse
import Network.MPD.Commands.Types
import Network.MPD.Core
import Network.MPD.Utils

import Control.Monad.Error (throwError)
import Data.List (intersperse)

-- Run getResponse but discard the response.
getResponse_ :: MonadMPD m => String -> m ()
getResponse_ x = getResponse x >> return ()

-- Get the lines of the daemon's response to a list of commands.
getResponses :: MonadMPD m => [String] -> m [String]
getResponses cmds = getResponse . concat $ intersperse "\n" cmds'
    where cmds' = "command_list_begin" : cmds ++ ["command_list_end"]

-- Helper that throws unexpected error if input is empty.
failOnEmpty :: MonadMPD m => [String] -> m [String]
failOnEmpty [] = throwError $ Unexpected "Non-empty response expected."
failOnEmpty xs = return xs

-- A wrapper for getResponse that fails on non-empty responses.
getResponse1 :: MonadMPD m => String -> m [String]
getResponse1 x = getResponse x >>= failOnEmpty

-- Run 'toAssocList' and return only the values.
takeValues :: [String] -> [String]
takeValues = map snd . toAssocList

-- Build a list of Song instances from a response.
takeSongs :: MonadMPD m => [String] -> m [Song]
takeSongs = return . parseSongs . toAssocList

-- Build a list of Entry instances from a response.
takeEntries :: MonadMPD m => [String] -> m [Entry]
takeEntries = return . parseEntries . toAssocList
