{-# LANGUAGE MultiParamTypeClasses #-}
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

-- | Module    : Network.MPD.Prim
-- Copyright   : (c) Ben Sinclair 2005-2007
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : not Haskell 98 (uses MultiParamTypeClasses)
--
-- Core functionality.

module Network.MPD.Prim (
    -- * Data types
    MPD(..), Conn(..), MPDError(..), ACKType(..), Response,
    -- * Interacting
    getResponse, close, reconnect, kill,
    ) where

import Control.Monad (liftM)
import Control.Monad.Error (Error(..), MonadError(..))
import Control.Monad.Trans
import Prelude hiding (repeat)
import Data.List (isPrefixOf)
import Data.Maybe
import System.IO

--
-- Data types.
--

-- A class of transports with which to connect to MPD servers.
data Conn = Conn { cOpen  :: IO ()                          -- Open connection
                 , cClose :: IO ()                          -- Close connection
                 , cSend  :: String -> IO (Response String) -- Write to connection
                 , cGetPW :: IO (Maybe String) }            -- Password function

-- | The MPDError type is used to signal errors, both from the MPD and
-- otherwise.
data MPDError = NoMPD              -- ^ MPD not responding
              | TimedOut           -- ^ The connection timed out
              | Custom String      -- ^ Used for misc. errors
              | ACK ACKType String -- ^ ACK type and a message from the
                                   --   server
                deriving Eq

instance Show MPDError where
    show NoMPD      = "Could not connect to MPD"
    show TimedOut   = "MPD connection timed out"
    show (Custom s) = s
    show (ACK _ s)  = s

-- | Represents various MPD errors (aka. ACKs).
data ACKType = InvalidArgument  -- ^ Invalid argument passed (ACK 2)
             | InvalidPassword  -- ^ Invalid password supplied (ACK 3)
             | Auth             -- ^ Authentication required (ACK 4)
             | UnknownCommand   -- ^ Unknown command (ACK 5)
             | FileNotFound     -- ^ File or directory not found ACK 50)
             | PlaylistMax      -- ^ Playlist at maximum size (ACK 51)
             | System           -- ^ A system error (ACK 52)
             | PlaylistLoad     -- ^ Playlist loading failed (ACK 53)
             | Busy             -- ^ Update already running (ACK 54)
             | NotPlaying       -- ^ An operation requiring playback
                                --   got interrupted (ACK 55)
             | FileExists       -- ^ File already exists (ACK 56)
             | UnknownACK       -- ^ An unknown ACK (aka. bug)
               deriving (Eq)

-- | A response is either an 'MPDError' or some result.
type Response a = Either MPDError a

-- Export the type name but not the constructor or the field.
-- | The MPD monad is basically a reader and error monad
-- combined.
--
-- To use the error throwing\/catching capabilities:
--
-- > import Control.Monad.Error
--
-- To run IO actions within the MPD monad:
--
-- > import Control.Monad.Trans
data MPD a = MPD { runMPD :: Conn -> IO (Response a) }

instance Functor MPD where
    fmap f m = MPD $ \conn -> either Left (Right . f) `liftM` runMPD m conn

instance Monad MPD where
    return a = MPD $ \_ -> return $ Right a
    m >>= f  = MPD $ \conn -> runMPD m conn >>=
                              either (return . Left) (flip runMPD conn . f)
    fail err = MPD $ \_ -> return . Left $ Custom err

instance MonadIO MPD where
    liftIO m = MPD $ \_ -> liftM Right m

instance MonadError MPDError MPD where
    throwError e   = MPD $ \_ -> return (Left e)
    catchError m h = MPD $ \conn ->
        runMPD m conn >>= either (flip runMPD conn . h) (return . Right)

instance Error MPDError where
    noMsg  = Custom "An error occurred"
    strMsg = Custom

--
-- Basic connection functions
--

-- | Refresh a connection.
reconnect :: MPD ()
reconnect = MPD $ \conn -> Right `liftM` cOpen conn

-- | Kill the server. Obviously, the connection is then invalid.
kill :: MPD ()
kill = getResponse "kill" `catchError` cleanup >> return ()
    where
      cleanup TimedOut = MPD $ \conn -> cClose conn >> return (Right [])
      cleanup x = throwError x >> return []

-- | Close an MPD connection.
close :: MPD ()
close = MPD $ \conn -> cClose conn >> return (Right ())

--
-- Sending messages and handling responses.
--

-- | Send a command to the MPD and return the result.
getResponse :: String -> MPD [String]
getResponse cmd = MPD f
    where
        f conn = catchAuth . either Left parseResponse =<< cSend conn cmd
            where
                catchAuth (Left (ACK Auth _)) = tryPassword conn (f conn)
                catchAuth x = return x

-- Send a password to MPD and run an action on success.
tryPassword :: Conn -> IO (Response a) -> IO (Response a)
tryPassword conn cont = do
    resp <- cGetPW conn >>= maybe failAuth (cSend conn . ("password " ++))
    case resp of
        Left  e -> return $ Left e
        Right x -> either (return . Left) (const cont) $ parseResponse x
    where failAuth = return . Left $ ACK Auth "Password required"

-- Consume response and return a Response.
parseResponse :: String -> Response [String]
parseResponse s | null xs                    = Left  $ NoMPD
                | isPrefixOf "ACK" (head xs) = Left  $ parseAck s
                | otherwise                  = Right $ takeWhile ("OK" /=) xs
    where xs = lines s

parseAck :: String -> MPDError
parseAck s = ACK ack msg
    where
        ack = case code of
                "2"  -> InvalidArgument
                "3"  -> InvalidPassword
                "4"  -> Auth
                "5"  -> UnknownCommand
                "50" -> FileNotFound
                "51" -> PlaylistMax
                "52" -> System
                "53" -> PlaylistLoad
                "54" -> Busy
                "55" -> NotPlaying
                "56" -> FileExists
                _    -> UnknownACK
        (code, _, msg) = splitAck s

-- Break an ACK into (error code, current command, message).
-- ACKs are of the form:
-- ACK [error@command_listNum] {current_command} message_text\n
splitAck :: String -> (String, String, String)
splitAck s = (code, cmd, msg)
    where (code, notCode) = between (== '[') (== '@') s
          (cmd, notCmd)   = between (== '{') (== '}') notCode
          msg             = drop 1 . snd $ break (== ' ') notCmd

          -- take whatever is between 'f' and 'g'.
          between f g xs  = let (_, y) = break f xs
                            in break g (drop 1 y)
