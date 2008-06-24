-- | Module    : Network.MPD.SocketConn
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- Connection over a network socket.

module Network.MPD.SocketConn (withMPDEx) where

import Network.MPD.Core hiding (close)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Network
import System.IO
import Control.Monad (liftM, unless)
import Data.List (isPrefixOf)
import System.IO.Error (isEOFError)
import Control.Exception (finally)
import qualified System.IO.UTF8 as U

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
        send'  = send hR
        cautiousSend' x =
            send' x >>= either (\e -> case e of TimedOut -> open' >> send' x
                                                _        -> return $ Left e)
                               (return . Right)
    open'
    runMPD m (Conn open' close' cautiousSend' getpw) `finally` close'

open :: String -> Integer -> IORef (Maybe Handle) -> IO ()
open host port hR = withSocketsDo $ do
    close hR
    h <- safeConnectTo host port
    writeIORef hR h
    maybe (return ()) (\_ -> checkConn hR >>= flip unless (close hR)) h

close :: IORef (Maybe Handle) -> IO ()
close hR = readIORef hR >>= maybe (return ()) sendClose >> writeIORef hR Nothing
    where sendClose h = catch (hPutStrLn h "close" >> hReady h >> hClose h)
                              (whenEOF $ writeIORef hR Nothing)

send :: IORef (Maybe Handle) -> String -> IO (Response String)
send hR str = do
    hM <- readIORef hR
    case hM of
        Nothing -> return $ Left NoMPD
        Just h -> do
                unless (null str) (U.hPutStrLn h str >> hFlush h)
                catch (getLines h [])
                      (whenEOF $ writeIORef hR Nothing >> return (Left TimedOut))
    where
        getLines handle acc = do
            l <- U.hGetLine handle
            if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                then return . Right . unlines . reverse $ l:acc
                else getLines handle (l:acc)

--
-- Helpers
--

-- Return a value if the error is an EOFError, otherwise throw the error again.
whenEOF :: IO a -> IOError -> IO a
whenEOF result err = if isEOFError err then result else ioError err

safeConnectTo :: String -> Integer -> IO (Maybe Handle)
safeConnectTo host port =
    catch (liftM Just . connectTo host . PortNumber $ fromInteger port)
          (const $ return Nothing)

-- Check that an MPD daemon is at the other end of a connection.
checkConn :: IORef (Maybe Handle) -> IO Bool
checkConn c = liftM (either (const False) (isPrefixOf "OK MPD")) $ send c ""
