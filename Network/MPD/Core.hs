{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module    : Network.MPD.Core
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
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
    getResponse, kill,
    ) where

import           Network.MPD.Util
import           Network.MPD.Core.Class
import           Network.MPD.Core.Error

import           Data.Char (isDigit)
import           Control.Applicative (Applicative(..), (<$>), (<*))
import qualified Control.Exception as E
import           Control.Monad (ap, unless)
import           Control.Monad.Error (ErrorT(..), MonadError(..))
import           Control.Monad.Reader (ReaderT(..), ask)
import           Control.Monad.State (StateT, MonadIO(..), modify, get, evalStateT)
import qualified Data.Foldable as F
import           Network (PortID(..), withSocketsDo, connectTo)
import           System.IO (Handle, hPutStrLn, hReady, hClose, hFlush)
import           System.IO.Error (isEOFError)
import qualified System.IO.UTF8 as U
import           Text.Printf (printf)

import qualified Prelude
import           Prelude hiding (break, drop, dropWhile, read)
import           Data.ByteString.Char8 (ByteString, isPrefixOf, break, drop, dropWhile)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8

--
-- Data types.
--

type Host = String
type Port = Integer

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
                    (StateT MPDState
                     (ReaderT (Host, Port) IO)) a
        } deriving (Functor, Monad, MonadIO, MonadError MPDError)

instance Applicative MPD where
    (<*>) = ap
    pure  = return

instance MonadMPD MPD where
    open  = mpdOpen
    close = mpdClose
    send  = mpdSend
    getHandle      = MPD $ stHandle <$> get
    getPassword    = MPD $ stPassword <$> get
    setPassword pw = MPD $ modify (\st -> st { stPassword = pw })
    getVersion     = MPD $ stVersion <$> get

-- | Inner state for MPD
data MPDState =
    MPDState { stHandle   :: Maybe Handle
             , stPassword :: String
             , stVersion  :: (Int, Int, Int)
             }

-- | A response is either an 'MPDError' or some result.
type Response = Either MPDError

-- | The most configurable API for running an MPD action.
withMPDEx :: Host -> Port -> Password -> MPD a -> IO (Response a)
withMPDEx host port pw x = withSocketsDo $
    runReaderT (evalStateT (runErrorT . runMPD $ open >> (x <* close)) initState)
               (host, port)
    where initState = MPDState Nothing pw (0, 0, 0)

mpdOpen :: MPD ()
mpdOpen = MPD $ do
    (host, port) <- ask
    runMPD close
    handle <- liftIO (safeConnectTo host port)
    modify (\st -> st { stHandle = handle })
    F.forM_ handle (const $ runMPD checkConn >>= flip unless (runMPD close))
    where
        safeConnectTo host@('/':_) _ =
            (Just <$> connectTo "" (UnixSocket host))
            `E.catch` (\(_ :: E.SomeException) -> return Nothing)
        safeConnectTo host port =
            (Just <$> connectTo host (PortNumber $ fromInteger port))
            `E.catch` (\(_ :: E.SomeException) -> return Nothing)
        checkConn = do
            [msg] <- send ""
            if "OK MPD" `isPrefixOf` msg
                then MPD $ checkVersion $ parseVersion msg
                else return False

        checkVersion Nothing = throwError $ Custom "Couldn't determine MPD version"
        checkVersion (Just version)
            | version < requiredVersion =
                throwError $ Custom $ printf
                    "MPD %s is not supported, upgrade to MPD %s or above!"
                    (formatVersion version) (formatVersion requiredVersion)
            | otherwise = do
                modify (\st -> st { stVersion = version })
                return True
            where
                requiredVersion = (0, 15, 0)

        parseVersion = parseTriple '.' parseNum . dropWhile (not . isDigit)

        formatVersion :: (Int, Int, Int) -> String
        formatVersion (x, y, z) = printf "%d.%d.%d" x y z


mpdClose :: MPD ()
mpdClose =
    MPD $ do
        get >>= F.mapM_ (liftIO . sendClose) . stHandle
        modify (\st -> st { stHandle = Nothing })
    where
        sendClose handle =
            (hPutStrLn handle "close" >> hReady handle >> hClose handle)
            `E.catch` whenEOF (return ())

        whenEOF result err
            | isEOFError err = result
            | otherwise      = ioError err

mpdSend :: String -> MPD [ByteString]
mpdSend str = send' `catchError` handler
    where
        handler TimedOut = mpdOpen >> send'
        handler err      = throwError err

        send' = MPD $ get >>= maybe (throwError NoMPD) go . stHandle

        go handle = do
            unless (null str) $
                liftIO $ U.hPutStrLn handle str >> hFlush handle
            liftIO ((Right <$> getLines handle []) `E.catch` (return . Left))
                >>= either (\err -> if isEOFError err then
                                        modify (\st -> st { stHandle = Nothing })
                                        >> throwError TimedOut
                                      else liftIO (ioError err))
                           return

        getLines :: Handle -> [ByteString] -> IO [ByteString]
        getLines handle acc = do
            l <- B.hGetLine handle
            if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                then (return . reverse) (l:acc)
                else getLines handle (l:acc)


--
-- Other operations.
--

ignore :: (Monad m) => m a -> m ()
ignore x = x >> return ()

-- | Kill the server. Obviously, the connection is then invalid.
kill :: (MonadMPD m) => m ()
kill = ignore (send "kill") `catchError` cleanup
    where
        cleanup e = if e == TimedOut then close else throwError e

-- | Send a command to the MPD server and return the result.
getResponse :: (MonadMPD m) => String -> m [ByteString]
getResponse cmd = (send cmd >>= parseResponse) `catchError` sendpw
    where
        sendpw e@(ACK Auth _) = do
            pw <- getPassword
            if null pw then throwError e
                else send ("password " ++ pw) >>= parseResponse
                  >> send cmd >>= parseResponse
        sendpw e =
            throwError e

-- Consume response and return a Response.
parseResponse :: (MonadError MPDError m) => [ByteString] -> m [ByteString]
parseResponse xs
    | null xs                    = throwError $ NoMPD
    | "ACK" `isPrefixOf` x       = throwError $ parseAck x
    | otherwise                  = return $ Prelude.takeWhile ("OK" /=) xs
    where
        x = head xs

-- Turn MPD ACK into the corresponding 'MPDError'
parseAck :: ByteString -> MPDError
parseAck s = ACK ack (UTF8.toString msg)
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
splitAck :: ByteString -> (Int, ByteString, ByteString)
splitAck s = (read code, cmd, msg)
    where
        (code, notCode) = between '[' '@' s
        (cmd, notCmd)   = between '{' '}' notCode
        msg             = drop 1 $ dropWhile (' ' ==) notCmd

        -- take whatever is between 'f' and 'g'.
        between a b xs  = let (_, y) = break (== a) xs
                          in break (== b) (drop 1 y)
