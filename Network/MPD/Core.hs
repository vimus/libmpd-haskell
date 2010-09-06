{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

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

import Network.MPD.Utils
import Network.MPD.Core.Class
import Network.MPD.Core.Error

import Char (isDigit)
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap, unless)
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.State (StateT, MonadIO(..), modify, get, evalStateT)
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
    receive = mpdReceive
    getHandle = MPD $ get >>= return . stHandle
    getPassword = MPD $ get >>= return . stPassword
    setPassword pw = MPD $ modify (\st -> st { stPassword = pw })
    getVersion = MPD $ get >>= return . stVersion

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
    runReaderT (evalStateT (runErrorT . runMPD $ open >> x) initState)
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
            `catch` const (return Nothing)
        safeConnectTo host port =
            (Just <$> connectTo host (PortNumber $ fromInteger port))
            `catch` const (return Nothing)

        checkConn = do
            msg <- mpdReceive >>= checkMsg
            if isPrefixOf "OK MPD" msg
               then do
                   MPD $ maybe (throwError $ Custom "Couldn't determine MPD version")
                               (\v -> modify (\st -> st { stVersion = v }))
                               (parseVersion msg)
                   return True
               else return False

        checkMsg ls =
            if null ls
               then throwError $ Custom "No welcome message"
               else return $ head ls

        parseVersion s =
            case (parseNum v1, parseNum v2, parseNum v3) of
                 (Just v1', Just v2', Just v3') -> Just (v1', v2', v3')
                 _                              -> Nothing
            where (v1, s2) = breakChar '.' s1
                  (v2, v3) = breakChar '.' s2
                  s1       = dropWhile (not . isDigit) s

mpdClose :: MPD ()
mpdClose =
    MPD $ do
        get >>= F.mapM_ (liftIO . sendClose) . stHandle
        modify (\st -> st { stHandle = Nothing })
    where
        sendClose handle =
            (hPutStrLn handle "close" >> hReady handle >> hClose handle)
            `catch` whenEOF (return ())

        whenEOF result err
            | isEOFError err = result
            | otherwise      = ioError err

mpdSend :: String -> MPD ()
mpdSend str = MPD $ get >>= maybe (throwError NoMPD) go . stHandle
    where
        go handle =
            unless (null str) $
                liftIO $ U.hPutStrLn handle str >> hFlush handle

mpdReceive :: MPD [String]
mpdReceive = getHandle >>= maybe (throwError NoMPD) recv
    where
        recv handle = MPD $
            liftIO ((Right <$> getLines handle []) `catch` (return . Left))
                >>= either (\err -> if isEOFError err then
                                        modify (\st -> st { stHandle = Nothing })
                                        >> throwError TimedOut
                                      else liftIO (ioError err))
                           return

        getLines handle acc = do
            l <- U.hGetLine handle
            if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                then return $ reverse (l:acc)
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
getResponse cmd = (send cmd >> receive >>= parseResponse) `catchError` sendpw
    where
        sendpw e@(ACK Auth _) = do
            pw <- getPassword
            if null pw
                then throwError e
                else do send ("password " ++ pw) >> receive >>= parseResponse
                        send cmd                 >> receive >>= parseResponse
        sendpw e =
            throwError e

-- Consume response and return a Response.
parseResponse :: (MonadError MPDError m) => [String] -> m [String]
parseResponse xs
    | null xs                    = throwError $ NoMPD
    | isPrefixOf "ACK" (head xs) = throwError $ parseAck (head xs)
    | otherwise                  = return $ Prelude.takeWhile ("OK" /=) xs

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
