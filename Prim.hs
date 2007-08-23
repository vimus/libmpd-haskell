{-
    libmpd for Haskell, a MPD client library.
    Copyright (C) 2005  Ben Sinclair <bsinclai@turing.une.edu.au>

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

-- | Module    : Prim
-- Copyright   : (c) Ben Sinclair 2005
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- MPD client library.

module Prim (

             -- * Data types
             MPD, ACK(..),

             -- * Running an action
             withMPD, withMPD_,

             -- * Errors
             catchMPD,

             -- * Interacting
             getResponse, clearerror, close, reconnect, kill,

             module Control.Monad.Trans
            ) where

import Control.Monad (liftM)
import Control.Exception (finally)
import Control.Monad.Trans
import Prelude hiding (repeat)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe
import Network
import System.IO

--
-- Data types.
--

-- | A connection to a MPD server.
-- don't export the field names.
data Connection = Conn { connHostName :: String
                       , connPortNum  :: Integer
                       , connHandle   :: Handle
                       , connGetPass  :: IO (Maybe String)
                       }

-- | Represents various MPD errors (aka. ACKs).
data ACK = NoMPD                 -- ^ MPD not responding
         | Auth                  -- ^ ACK [4\@0]
         | Busy                  -- ^ ACK [54\@0]
         | UnknownCommand String -- ^ ACK [5\@0]
         | Custom String

instance Show ACK where
    show NoMPD              = "Could not connect to MPD"
    show Auth               = "Password needed"
    show Busy               = "Already updating"
    show (UnknownCommand s) = "Unknown command: " ++ s
    show (Custom s)         = s



-- Export the type name but not the constructor or the field.
--
-- This is basically a state and an error monad combined. It's just
-- nice if we can have a few custom functions that fiddle with the
-- internals.
newtype MPD a = MPD { runMPD :: IORef Connection -> IO (Either ACK a) }

instance Functor MPD where
    fmap f m = MPD $ \cRef -> either Left (Right . f) `liftM` runMPD m cRef

instance Monad MPD where
    return a = MPD $ \_ -> return (Right a)
    m >>= f  = MPD $ \cRef -> runMPD m cRef >>=
                              either (return . Left) (flip runMPD cRef . f)
    fail err = MPD $ \_ -> return $ Left (Custom err)

instance MonadIO MPD where
    liftIO m = MPD $ \_ -> liftM Right m


{-
-- I've skipped throwMPD because it doesn't really make sense for ACKs
-- to be thrown from user code.

-- | Throw an exception.
throwMPD :: ACK -> MPD ()
throwMPD e = MPD $ \_ -> return (Left e)
-}

-- | Catch an exception from an action.
catchMPD :: MPD a -> (ACK -> MPD a) -> MPD a
catchMPD m h = MPD $ \cRef ->
    runMPD m cRef >>= either (flip runMPD cRef . h) (return . Right)


{-
--
-- Basic connection functions
--
-}

-- | Run an MPD action against a server.
withMPD :: String            -- ^ Host name.
        -> Integer           -- ^ Port number.
        -> IO (Maybe String) -- ^ An action that supplies passwords.
        -> MPD a             -- ^ The action to run.
        -> IO (Either ACK a)
withMPD host port getpw m =
    connect host port getpw >>= maybe (return $ Left NoMPD) withConn
    where withConn c = do r <- newIORef c
                          finally (runMPD m r) (readIORef r >>= closeIO)

-- | Run an MPD action against a server with no provision for passwords.
withMPD_ :: String -> Integer -> MPD a -> IO (Either ACK a)
withMPD_ = flip (flip . withMPD) (return Nothing)

-- Connect to an MPD server.
connect :: String -> Integer -- host and port
        -> IO (Maybe String) -- a password suppplier
        -> IO (Maybe Connection)
connect host port getpw =
    withSocketsDo $ do
        handle <- connectTo host . PortNumber $ fromInteger port
        conn   <- return $ Conn host port handle getpw
        mpd    <- checkConn conn
        if mpd then return (Just conn) else closeIO conn >> return Nothing

-- Check that an MPD daemon is at the other end of a connection.
checkConn :: Connection -> IO Bool
checkConn conn = isPrefixOf "OK MPD" `liftM` hGetLine (connHandle conn)

-- Close a connection.
closeIO :: Connection -> IO ()
closeIO conn = hPutStrLn h "close" >> hClose h
    where h = connHandle conn

-- Refresh a connection.
reconnect :: MPD ()
reconnect = MPD $ \cRef -> do
    conn@(Conn host port _ getpw) <- readIORef cRef
    closeIO conn >> connect host port getpw >>=
        maybe (return $ Left NoMPD)
              (\conn' -> writeIORef cRef conn' >> return (Right ()))

-- | Kill the server. Obviously, the connection is then invalid.
kill :: MPD ()
kill  = MPD $ \cRef -> do c <- readIORef cRef
                          let h = connHandle c
                          hPutStrLn h "kill" >> hClose h
                          return (Left NoMPD)

-- | Send a command to the MPD and return the result.
getResponse :: String -> MPD [String]
getResponse cmd = MPD $ \cRef -> do
    -- XXX put in connection testing stuff here.
    -- if False then runMPD reconnect cRef else return (Right ())
    conn <- readIORef cRef
    let h = connHandle conn
    hPutStrLn h cmd >> hFlush h
    loop h (tryPassword cRef (getResponse cmd)) []
    where loop h tryPw acc = do
              l <- hGetLine h
              parseResponse (loop h tryPw) l acc >>= either
                  (\x -> case x of Auth -> tryPw; _ -> return (Left x))
                  (return . Right)

-- Send a password to MPD and run an action on success, return an ACK
-- on failure.
tryPassword :: IORef Connection
            -> MPD a  -- run on success
            -> IO (Either ACK a)
tryPassword cRef cont = do
    conn <- readIORef cRef
    let h = connHandle conn
    connGetPass conn >>= maybe (return $ Left Auth)
        (\pw -> do hPutStrLn h ("password " ++ pw) >> hFlush h
                   result <- hGetLine h
                   case result of "OK" -> runMPD cont cRef
                                  _    -> tryPassword cRef cont)

splitAck :: String -> (String, String, String)
splitAck s = (take 3 prefix, code, drop 2 msg)
  where
    (_, msg)       = break (== '}') msg'
    (code, msg')   = break (== ' ') rest
    (prefix, rest) = splitAt 4 s

-- > parseAck "ACK [5@0] {} unknown command \"pong\"" = Custom "unknown
-- command \"pong\""
parseAck :: String -> ACK
parseAck s = case code of
                  "[4@0]"  -> Auth
                  "[54@0]" -> Busy
                  _       -> Custom msg
    where (_, code, msg) = splitAck s

-- Consume response and return a Response.
parseResponse :: ([String] -> IO (Either ACK [String]))
              -> String -> [String] -> IO (Either ACK [String])
parseResponse f s acc
    | isPrefixOf "ACK" s = return $ Left (parseAck s)
    | isPrefixOf "OK" s  = return $ Right (reverse acc)
    | otherwise          = f (s:acc)

-- | Clear the current error message in status.
clearerror :: MPD ()
clearerror = MPD $ \cRef -> do
    (Conn _ _ h _) <- readIORef cRef
    hPutStrLn h "clearerror" >> hClose h
    return (Right ())

-- should this return an error?
--
-- | Close a MPD connection.
close :: MPD ()
close = MPD $ \cRef -> readIORef cRef >>= closeIO >> return (Right ())
