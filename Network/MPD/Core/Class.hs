{-# LANGUAGE FlexibleContexts #-}

-- | Module    : Network.MPD.Core.Class
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- The MPD typeclass.

module Network.MPD.Core.Class where

import           System.IO (Handle)
import           Data.ByteString (ByteString)

import           Network.MPD.Core.Error (MPDError)

import           Control.Monad.Error (MonadError)

type Password = String

-- | A typeclass to allow for multiple implementations of a connection
--   to an MPD server.
class (Monad m, MonadError MPDError m) => MonadMPD m where
    -- | Open (or re-open) a connection to the MPD server.
    open  :: m ()
    -- | Close the connection.
    close :: m ()
    -- | Send a string to the server and return its response.
    send  :: String -> m [ByteString]
    -- | Get underlying Handle (or Nothing, if no connection is estabilished)
    getHandle :: m (Maybe Handle)
    -- | Produce a password to send to the server should it ask for
    --   one.
    getPassword :: m Password
    -- | Alters password to be sent to the server.
    setPassword :: Password -> m ()
    -- | Get MPD protocol version
    getVersion :: m (Int, Int, Int)
