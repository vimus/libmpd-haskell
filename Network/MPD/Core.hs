{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

-- | Module    : Network.MPD.Core
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : not Haskell 98 (uses flexible contexts and cunning
--               generalised newtype deriving)
--
-- The core datatypes and operations are defined here, including the
-- primary instance of the 'MonadMPD' class, 'MPD'.

module Network.MPD.Core (
    -- * Classes
    MonadMPD(..),
    -- * Data types
    MPD, MPDError(..), ACKType(..), Response, Host, Port, Password,
    -- * Running
    withMPDEx,
    -- * Interacting
    getResponse, reconnect, kill,
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap, unless)
import Control.Monad.Error (Error(..), ErrorT(..), MonadError(..))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State (StateT, MonadIO(..), put, get, evalStateT)
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)
import Network (PortID(..), withSocketsDo, connectTo)
import System.IO (Handle, hPutStrLn, hReady, hClose, hFlush)
import System.IO.Error (isEOFError)
import qualified System.IO.UTF8 as U

--
-- Data types.
--

type Host = String
type Port = Integer
type Password = String

-- | The MPDError type is used to signal errors, both from the MPD and
-- otherwise.
data MPDError = NoMPD              -- ^ MPD not responding
              | TimedOut           -- ^ The connection timed out
              | Unexpected String  -- ^ MPD returned an unexpected response.
                                   --   This is a bug, either in the library or
                                   --   in MPD itself.
              | Custom String      -- ^ Used for misc. errors
              | ACK ACKType String -- ^ ACK type and a message from the
                                   --   server
                deriving Eq

instance Show MPDError where
    show NoMPD          = "Could not connect to MPD"
    show TimedOut       = "MPD connection timed out"
    show (Unexpected s) = "MPD returned an unexpected response: " ++ s
    show (Custom s)     = s
    show (ACK _ s)      = s

instance Error MPDError where
    noMsg  = Custom "An error occurred"
    strMsg = Custom

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

-- | A typeclass to allow for multiple implementations of a connection
--   to an MPD server.
class (Monad m, MonadError MPDError m) => MonadMPD m where
    -- | Open (or re-open) a connection to the MPD server.
    open  :: m ()
    -- | Close the connection.
    close :: m ()
    -- | Send a string to the server and return its response.
    send  :: String -> m String
    -- | Produce a password to send to the server should it ask for
    --   one.
    getPassword :: m Password

--
-- IO based MPD client implementation.
--

-- | The main implementation of an MPD client.  It actually connects
--   to a server and interacts with it.
--
-- To use the error throwing\/catching capabilities:
--
-- > import Control.Monad.Error (throwError, catchError)
--
-- To run IO actions within the MPD monad:
--
-- > import Control.Monad.Trans (liftIO)
newtype MPD a =
    MPD { runMPD :: ErrorT MPDError
                    (StateT (Maybe Handle)
                     (ReaderT (Host, Port, Password) IO)) a
        } deriving (Functor, Monad, MonadIO, MonadError MPDError)

instance Applicative MPD where
    (<*>) = ap
    pure  = return

instance MonadMPD MPD where
    open  = mpdOpen
    close = mpdClose
    send  = mpdSend
    getPassword = MPD $ ask >>= \(_,_,pw) -> return pw

-- | A response is either an 'MPDError' or some result.
type Response a = Either MPDError a

-- | The most configurable API for running an MPD action.
withMPDEx :: Host -> Port -> Password -> MPD a -> IO (Response a)
withMPDEx host port pw x = withSocketsDo $
    runReaderT (evalStateT (runErrorT . runMPD $ open >> x) Nothing)
               (host, port, pw)

mpdOpen :: MPD ()
mpdOpen = MPD $ do
    (host, port, _) <- ask
    runMPD close
    handle <- liftIO (safeConnectTo host port)
    put handle
    maybe (return ())
          (\_ -> runMPD checkConn >>= flip unless (runMPD close))
          handle
    where
        safeConnectTo host port =
            (Just <$> connectTo host (PortNumber $ fromInteger port))
            `catch`
            (const $ return Nothing)

        checkConn =
            isPrefixOf "OK MPD" <$> send ""

mpdClose :: MPD ()
mpdClose =
    MPD $ get >>= maybe (return ()) (liftIO . sendClose) >> put Nothing
    where
        sendClose handle =
            (hPutStrLn handle "close" >> hReady handle >> hClose handle)
            `catch`
            (whenEOF $ return ())

        whenEOF result err
            | isEOFError err = result
            | otherwise      = ioError err

mpdSend :: String -> MPD String
mpdSend str = MPD $ get >>= maybe (throwError NoMPD) go
    where
        go handle = do
            unless (null str) $
                liftIO $ U.hPutStrLn handle str >> hFlush handle

            r <- liftIO $ (Right <$> getLines handle []) `catch` (return . Left)
            case r of
                Right result -> return result
                Left err | isEOFError err -> put Nothing >> throwError NoMPD
                         | otherwise      -> liftIO $ ioError err

        getLines handle acc = do
            l <- U.hGetLine handle
            if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                then return . unlines $ reverse (l:acc)
                else getLines handle (l:acc)


--
-- Other operations.
--

-- | Refresh a connection.
reconnect :: MonadMPD m => m ()
reconnect = open

-- | Kill the server. Obviously, the connection is then invalid.
kill :: (MonadMPD m) => m ()
kill = (send "kill" `catchError` cleanup) >> return ()
    where
        cleanup TimedOut = close >> return ""
        cleanup x = throwError x

-- | Send a command to the MPD server and return the result.
getResponse :: (MonadMPD m) => String -> m [String]
getResponse cmd = (send cmd >>= parseResponse) `catchError` sendpw
    where
        sendpw (ACK Auth _) =
            getPassword >>= (send . ("password "++)) >>= parseResponse
        sendpw x =
            throwError x

-- Consume response and return a Response.
parseResponse :: (MonadError MPDError m) => String -> m [String]
parseResponse s
    | null xs                    = throwError $ NoMPD
    | isPrefixOf "ACK" (head xs) = throwError $ parseAck s
    | otherwise                  = return $ Prelude.takeWhile ("OK" /=) xs
    where
        xs = lines s

-- Turn MPD ACK into the corresponding 'MPDError'
parseAck :: String -> MPDError
parseAck s = ACK ack msg
    where
        ack = case code of
                2  -> InvalidArgument
                3  -> InvalidPassword
                4  -> Auth
                5  -> UnknownCommand
                50 -> FileNotFound
                51 -> PlaylistMax
                52 -> System
                53 -> PlaylistLoad
                54 -> Busy
                55 -> NotPlaying
                56 -> FileExists
                _  -> UnknownACK
        (code, _, msg) = splitAck s

-- Break an ACK into (error code, current command, message).
-- ACKs are of the form:
-- ACK [error@command_listNum] {current_command} message_text\n
splitAck :: String -> (Int, String, String)
splitAck s = (read code, cmd, msg)
    where
        (code, notCode) = between '[' '@' s
        (cmd, notCmd)   = between '{' '}' notCode
        msg             = drop 1 $ dropWhile (' ' ==) notCmd

        -- take whatever is between 'f' and 'g'.
        between a b xs  = let (_, y) = break (== a) xs
                          in break (== b) (drop 1 y)
