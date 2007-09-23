{-
    libmpd for Haskell, an MPD client library.
    Copyright (C) 2005-2007  Ben Sinclair <bsinclai@turing.une.edu.au>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

-- | Module    : Network.MPD
-- Copyright   : (c) Ben Sinclair 2005-2007
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- MPD client library.

module Network.MPD (
    -- * Basic data types
    MPD, ACK(..), ACKType(..), Response,
    module Network.MPD.Commands,
    -- * Connections
    withMPD, withMPDEx,
    -- * Misc.
    throwMPD, catchMPD
    ) where

import Network.MPD.Commands
import Network.MPD.Prim

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Data.IORef (newIORef, atomicModifyIORef)
import System.Environment (getEnv)
import System.IO
import System.IO.Error (isDoesNotExistError, ioError)

-- | Run an MPD action using localhost:6600 as the default host:port,
-- or whatever is found in the environment variables MPD_HOST and
-- MPD_PORT. If MPD_HOST is of the form \"password\@host\" then the
-- password will be supplied as well.
withMPD :: MPD a -> IO (Response a)
withMPD m = do
    port <- liftM read (getEnvDefault "MPD_PORT" "6600")
    (pw,host) <- liftM (break (== '@')) (getEnvDefault "MPD_HOST" "localhost")
    let (host',pw') = if null host then (pw,host) else (drop 1 host,pw)
    pwGen <- mkPasswordGen [pw']
    withMPDEx host' port pwGen m
    where
        getEnvDefault x dflt =
            catch (getEnv x) (\e -> if isDoesNotExistError e
                                    then return dflt else ioError e)

-- | Create an action that produces passwords for a connection. You
-- can pass these to 'withMPDEx' and it will use them to get passwords
-- to send to the server until one works or it runs out of them.
--
-- > do gen <- mkPasswordGen ["password1", "password2"]
-- >    withMPDEx "localhost" 6600 gen (update [])
mkPasswordGen :: [String] -> IO (IO (Maybe String))
mkPasswordGen = liftM f . newIORef
    where f = flip atomicModifyIORef $ \xs -> (drop 1 xs, listToMaybe xs)
