{- |
Module      : Network.MPD.Commands.Connection
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : MIT (see LICENSE)

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

Connection settings.
-}

module Network.MPD.Commands.Connection
    ( password
    , ping
    ) where

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.Connection as A
import           Network.MPD.Core

-- XXX should the password be quoted? Change "++" to "<@>" if so.  If
--     it should, it also needs to be fixed in N.M.Core.
-- | Send password to server to authenticate session.
-- Password is sent as plain text.
password :: MonadMPD m => String -> m ()
password = A.runCommand . A.password

-- | Check that the server is still responding.
ping :: MonadMPD m => m ()
ping = A.runCommand A.ping
