{-
    libmpd for Haskell, an MPD client library.
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
             withMPDEx,

             -- * Errors
             throwMPD, catchMPD,

             -- * Interacting
             getResponse, clearerror, close, reconnect, kill,

             module Control.Monad.Trans
            ) where

import Control.Monad (liftM, unless)
import Control.Exception (finally)
import Control.Monad.Trans
import Prelude hiding (repeat)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe
import Network
import System.IO
import System.IO.Error (isEOFError)

--
-- Data types.
--

-- | A connection to an MPD server.
-- don't export the field names.
data Connection = Conn { connHostName :: String
                       , connPortNum  :: Integer
                       , connHandle   :: IORef (Maybe Handle)
                       , connGetPass  :: IO (Maybe String)
                       }

-- | Represents various MPD errors (aka. ACKs).
data ACK = NoMPD                 -- ^ MPD not responding
         | TimedOut              -- ^ The connection timed out.
         | Auth                  -- ^ ACK [4\@0]
         | Busy                  -- ^ ACK [54\@0]
         | UnknownCommand String -- ^ ACK [5\@0]
         | FileNotFound String   -- ^ ACK [50\@0]
         | FileExists String     -- ^ ACK [56\@0]
         | System                -- ^ ACK [52\@0]
         | PlaylistLoad          -- ^ ACK [53\@0]
         | NotPlaying            -- ^ ACK [55\@0]
         | PlaylistMax           -- ^ ACK [51\@0]
         | InvalidArgument       -- ^ ACK [2\@0]
         | InvalidPassword       -- ^ ACK [3\@0]
         | Custom String

instance Show ACK where
    show NoMPD              = "Could not connect to MPD"
    show TimedOut           = "MPD connection timed out"
    show Auth               = "Password needed"
    show Busy               = "Already updating"
    show (UnknownCommand s) = "Unknown command: " ++ s
    show (FileNotFound s)   = "File or directory does not exist: " ++ s
    show (FileExists s)     = "File or directory already exists: " ++ s
    show System             = "System error"
    show PlaylistLoad       = "Failed to load playlist"
    show PlaylistMax        = "Playlist full"
    show InvalidArgument    = "Invalid argument"
    show InvalidPassword    = "Invalid password"
    show NotPlaying         = "Playback stopped"
    show (Custom s)         = s

-- Export the type name but not the constructor or the field.
--
-- This is basically a state and an error monad combined. It's just
-- nice if we can have a few custom functions that fiddle with the
-- internals.
newtype MPD a = MPD { runMPD :: Connection -> IO (Either ACK a) }

instance Functor MPD where
    fmap f m = MPD $ \conn -> either Left (Right . f) `liftM` runMPD m conn

instance Monad MPD where
    return a = MPD $ \_ -> return (Right a)
    m >>= f  = MPD $ \conn -> runMPD m conn >>=
                              either (return . Left) (flip runMPD conn . f)
    fail err = MPD $ \_ -> return $ Left (Custom err)

instance MonadIO MPD where
    liftIO m = MPD $ \_ -> liftM Right m

-- | Throw an exception.
throwMPD :: ACK -> MPD ()
throwMPD e = MPD $ \_ -> return (Left e)

-- | Catch an exception from an action.
catchMPD :: MPD a -> (ACK -> MPD a) -> MPD a
catchMPD m h = MPD $ \conn ->
    runMPD m conn >>= either (flip runMPD conn . h) (return . Right)

{-
--
-- Basic connection functions
--
-}

-- | Run an MPD action against a server.
withMPDEx :: String            -- ^ Host name.
          -> Integer           -- ^ Port number.
          -> IO (Maybe String) -- ^ An action that supplies passwords.
          -> MPD a             -- ^ The action to run.
          -> IO (Either ACK a)
withMPDEx host port getpw m = do
    hRef <- newIORef Nothing
    connect host port hRef
    readIORef hRef >>= maybe (return $ Left NoMPD)
        (\_ -> finally (runMPD m (Conn host port hRef getpw)) (closeIO hRef))

-- Connect to an MPD server.
connect :: String -> Integer -- host and port
        -> IORef (Maybe Handle) -> IO ()
connect host port hRef =
    withSocketsDo $ do
        closeIO hRef
        handle <- connectTo host . PortNumber $ fromInteger port
        writeIORef hRef (Just handle)
        checkConn handle >>= flip unless (closeIO hRef)

-- Check that an MPD daemon is at the other end of a connection.
checkConn :: Handle -> IO Bool
checkConn h = isPrefixOf "OK MPD" `liftM` hGetLine h

-- Close a connection.
closeIO :: IORef (Maybe Handle) -> IO ()
closeIO hRef = do
    readIORef hRef >>= maybe (return ())
                             (\h -> hPutStrLn h "close" >> hClose h)
    writeIORef hRef Nothing

-- | Refresh a connection.
reconnect :: MPD ()
reconnect = MPD $ \(Conn host port hRef _) -> do
    connect host port hRef
    liftM (maybe (Left NoMPD) (const $ Right ())) (readIORef hRef)

-- XXX this doesn't use the password supplying feature.
--
-- | Kill the server. Obviously, the connection is then invalid.
kill :: MPD ()
kill = MPD $ \conn -> do
    readIORef (connHandle conn) >>=
        maybe (return ()) (\h -> hPutStrLn h "kill" >> hClose h)
    writeIORef (connHandle conn) Nothing
    return (Left NoMPD)

-- | Send a command to the MPD and return the result.
getResponse :: String -> MPD [String]
getResponse cmd = MPD $ \conn -> do
    readIORef (connHandle conn) >>=
        maybe (return $ Left NoMPD)
              (\h -> hPutStrLn h cmd >> hFlush h >>
                     loop h (tryPassword conn (getResponse cmd)) [])
    where loop h tryPw acc = do
              getln h (\l -> parseResponse (loop h tryPw) l acc >>= either
                          (\x -> case x of Auth -> tryPw; _ -> return (Left x))
                          (return . Right))
          getln h cont =
              catch (liftM Right $ hGetLine h) (return . Left) >>=
                  either (\e -> if isEOFError e then return (Left TimedOut)
                                                else ioError e)
                         cont

-- Send a password to MPD and run an action on success, return an ACK
-- on failure.
tryPassword :: Connection
            -> MPD a  -- run on success
            -> IO (Either ACK a)
tryPassword conn cont = do
    readIORef (connHandle conn) >>= maybe (return $ Left NoMPD)
        (\h -> connGetPass conn >>= maybe (return $ Left Auth)
                   (\pw -> do hPutStrLn h ("password " ++ pw) >> hFlush h
                              result <- hGetLine h
                              case result of "OK" -> runMPD cont conn
                                             _    -> tryPassword conn cont))

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
                  "[2@0]"  -> InvalidArgument
                  "[3@0]"  -> InvalidPassword
                  "[51@0]" -> PlaylistMax
                  "[52@0]" -> System
                  "[53@0]" -> PlaylistLoad
                  "[55@0]" -> NotPlaying
                  {- XXX need to extract what commands/files are
                  - unknown/missing/existing
                  "[5@0]"  -> UnknownCommand
                  "[50@0]" -> FileNotFound
                  "[56@0]" -> FileExists
                  -}
                  _        -> Custom msg
    where (_, code, msg) = splitAck s

-- Consume response and return a Response.
parseResponse :: ([String] -> IO (Either ACK [String]))
              -> String -> [String] -> IO (Either ACK [String])
parseResponse f s acc
    | isPrefixOf "ACK" s = return $ Left (parseAck s)
    | isPrefixOf "OK" s  = return $ Right (reverse acc)
    | otherwise          = f (s:acc)

-- XXX this doesn't use the password supplying feature.
--
-- | Clear the current error message in status.
clearerror :: MPD ()
clearerror = MPD $ \conn -> do
    readIORef (connHandle conn) >>= maybe (return $ Left NoMPD)
        (\h -> hPutStrLn h "clearerror" >> hFlush h >> return (Right ()))

-- | Close an MPD connection.
close :: MPD ()
close = MPD $ \conn -> closeIO (connHandle conn) >> return (Right ())
