{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

-- | Module    : Network.MPD.Core
-- Copyright   : (c) Ben Sinclair 2005-2009
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
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

import Network.MPD.Core.Class
import Network.MPD.Core.Error

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap, unless)
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State (StateT, MonadIO(..), put, get, evalStateT)
import qualified Data.Foldable as F
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
type Response = Either MPDError

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
    F.forM_ handle (const $ runMPD checkConn >>= flip unless (runMPD close))
    where
        safeConnectTo host port =
            (Just <$> connectTo host (PortNumber $ fromInteger port))
            `catch` const (return Nothing)

        checkConn =
            isPrefixOf "OK MPD" <$> send ""

mpdClose :: MPD ()
mpdClose =
    MPD $ get >>= F.mapM_ (liftIO . sendClose) >> put Nothing
    where
        sendClose handle =
            (hPutStrLn handle "close" >> hReady handle >> hClose handle)
            `catch` whenEOF (return ())

        whenEOF result err
            | isEOFError err = result
            | otherwise      = ioError err

mpdSend :: String -> MPD String
mpdSend str = send' `catchError` handler
    where
        handler TimedOut = mpdOpen >> send'
        handler err      = throwError err

        send' = MPD $ get >>= maybe (throwError NoMPD) go

        go handle = do
            unless (null str) $
                liftIO $ U.hPutStrLn handle str >> hFlush handle
            liftIO ((Right <$> getLines handle []) `catch` (return . Left))
                >>= either (\err -> if isEOFError err then
                                        put Nothing >> throwError TimedOut
                                      else liftIO (ioError err))
                           return

        getLines handle acc = do
            l <- U.hGetLine handle
            if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                then return . unlines $ reverse (l:acc)
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
getResponse :: (MonadMPD m) => String -> m [String]
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
