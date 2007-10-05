{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
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

module Network.MPD.SocketConn (MPD, SocketConn, withMPDEx) where

import Network.MPD.Prim
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network
import System.IO
import Control.Monad (liftM, unless)
import Data.List (isPrefixOf)
import System.IO.Error (isEOFError)
import Control.Monad.Error (MonadError(..))
import Control.Monad.Trans
import Control.Exception (finally)

-- | The AbstractMPD monad specialised to network connections. Almost
-- everybody will want this.
type MPD a = AbstractMPD SocketConn a

-- The field names should not be exported.
-- The accessors 'connPortNum' and 'connHandle' are not used, though
-- the fields are used (see 'reconnect').
-- | A network connection to an MPD server.
data SocketConn = SC String                 -- host name       
                     Integer                -- port number     
                     (IORef (Maybe Handle)) -- socket handle   
                     (IO (Maybe String))    -- password getter 

instance Conn SocketConn where
    connOpen  = scOpen
    connClose = scClose
    connRead  = scRead
    connWrite = scWrite
    connGetPW (SC _ _ _ pw) = pw

-- | Run an MPD action against a server.
withMPDEx :: String            -- ^ Host name.
          -> Integer           -- ^ Port number.
          -> IO (Maybe String) -- ^ An action that supplies passwords.
          -> MPD a             -- ^ The action to run.
          -> IO (Response a)
withMPDEx host port getpw m = do
    hRef <- newIORef Nothing
    let conn = SC host port hRef getpw
    connOpen conn 
    finally (runAbsMPD m conn) (connClose conn)

scOpen :: SocketConn -> IO ()
scOpen conn@(SC host port hRef _) =
    withSocketsDo $ do
    scClose conn
    handle <- safeConnectTo host port
    writeIORef hRef handle
    maybe (return ()) (\_ -> checkConn conn >>= flip unless (scClose conn))
          handle

scClose :: SocketConn -> IO ()
scClose (SC _ _ hRef _) =
    readIORef hRef >>= maybe (return ()) sendClose >> writeIORef hRef Nothing
    where
      sendClose h = catch (hPutStrLn h "close" >> hClose h)
                          (\err -> if isEOFError err then return ()
                                                     else ioError err)

scRead :: SocketConn -> IO (Response String)
scRead (SC _ _ hRef _) =
    readIORef hRef >>= maybe (return $ Left NoMPD) getTO
    where
      getTO  h = catch (liftM Right $ hGetLine h) markTO
      markTO e = if isEOFError e then (return $ Left TimedOut) else ioError e

scWrite :: SocketConn -> String -> IO (Response ())
scWrite (SC _ _ hRef _) str =
    readIORef hRef >>=
    maybe (return $ Left NoMPD)
          (\h -> hPutStrLn h str >> hFlush h >> return (Right ()))

--
-- Helpers
--

safeConnectTo :: String -> Integer -> IO (Maybe Handle)
safeConnectTo host port =
    catch (liftM Just . connectTo host . PortNumber $ fromInteger port)
          (const $ return Nothing)

-- Check that an MPD daemon is at the other end of a connection.
checkConn :: (Conn a) => a -> IO Bool
checkConn c = connRead c >>= return . either (const False) (isPrefixOf "OK MPD")
