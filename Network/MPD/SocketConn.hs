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

-- | Module    : Network.MPD.SocketConn
-- Copyright   : (c) Ben Sinclair 2005-2007
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- Connection over a network socket.

module Network.MPD.SocketConn (withMPDEx) where

import Network.MPD.Prim hiding (close)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network
import System.IO
import Control.Monad (liftM, unless)
import Data.List (isPrefixOf)
import System.IO.Error (isEOFError)
import Control.Exception (finally)

-- | Run an MPD action against a server.
withMPDEx :: String            -- ^ Host name.
          -> Integer           -- ^ Port number.
          -> IO (Maybe String) -- ^ An action that supplies passwords.
          -> MPD a             -- ^ The action to run.
          -> IO (Response a)
withMPDEx host port getpw m = do
    hR <- newIORef Nothing
    let open'  = open host port hR
        close' = close hR
        put'   = put hR
        get'   = get hR
    open'
    runMPD m (Conn open' close' put' get' getpw) `finally` close'

open :: String -> Integer -> IORef (Maybe Handle) -> IO ()
open host port hR = withSocketsDo $ do
    close hR
    h <- safeConnectTo host port
    writeIORef hR h
    maybe (return ()) (\_ -> checkConn hR >>= flip unless (close hR)) h

close :: IORef (Maybe Handle) -> IO ()
close hR = readIORef hR >>= maybe (return ()) sendClose >> writeIORef hR Nothing
    where sendClose h = catch (hPutStrLn h "close" >> hClose h)
                              (\err -> if isEOFError err then return ()
                                       else ioError err)

put :: IORef (Maybe Handle) -> String -> IO (Response ())
put hR str =
    readIORef hR >>=
    maybe (return $ Left NoMPD)
          (\h -> hPutStrLn h str >> hFlush h >> return (Right ()))

get :: IORef (Maybe Handle) -> IO (Response String)
get hR = readIORef hR >>= maybe (return $ Left NoMPD) getTO
    where
        getTO  h = catch (liftM Right $ hGetLine h) markTO
        markTO e = if isEOFError e then (return $ Left TimedOut) else ioError e

--
-- Helpers
--

safeConnectTo :: String -> Integer -> IO (Maybe Handle)
safeConnectTo host port =
    catch (liftM Just . connectTo host . PortNumber $ fromInteger port)
          (const $ return Nothing)

-- Check that an MPD daemon is at the other end of a connection.
checkConn :: IORef (Maybe Handle) -> IO Bool
checkConn = liftM (either (const False) (isPrefixOf "OK MPD")) . get
