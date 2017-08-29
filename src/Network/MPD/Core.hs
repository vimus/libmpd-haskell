{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies #-}

-- | Module    : Network.MPD.Core
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : MIT (see LICENSE)
-- Maintainer  : Joachim Fasting <joachifm@fastmail.fm>
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
import           Control.Concurrent.STM.TVar (TVar, readTVar, readTVarIO, writeTVar, newTVarIO)
import qualified Control.Exception as E
import           Control.Exception.Safe (catch, catchAny)
import           Control.Monad (ap, unless)
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Error (ErrorT(..), MonadError(..))
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Reader (ReaderT(..), ask, asks)
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import qualified Data.Foldable as F
import           Network (PortID(..), withSocketsDo, connectTo)
import           System.IO (Handle, hPutStrLn, hReady, hClose, hFlush)
import           System.IO.Error (isEOFError, tryIOError, ioeGetErrorType)
import           Text.Printf (printf)
import qualified GHC.IO.Exception as GE

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
                    (ReaderT MPDEnv IO) a
        } deriving (Functor, Monad, MonadIO, MonadError MPDError, MonadBase IO)

instance Applicative MPD where
    (<*>) = ap
    pure  = return

instance MonadBaseControl IO MPD where
  type StM MPD a = Either MPDError a
  liftBaseWith f = MPD $ liftBaseWith $ \q -> f (q . runMPD)
  restoreM = MPD . restoreM

instance MonadMPD MPD where
    open  = mpdOpen
    close = mpdClose
    send  = mpdSend
    getPassword = readEnvTVar envPassword
    setPassword = writeEnvTVar envPassword
    getVersion  = readEnvTVar envVersion

-- | Inner state for MPD
data MPDEnv =
    MPDEnv {   envHost     :: Host
             , envPort     :: Port
             , envHandle   :: TVar (Maybe Handle)
             , envPassword :: TVar String
             , envVersion  :: TVar (Int, Int, Int)
             }

readEnvTVar :: (MPDEnv -> TVar a) -> MPD a
readEnvTVar accessor = MPD $ liftIO . readTVarIO =<< asks accessor

writeEnvTVar :: (MPDEnv -> TVar a) -> a -> MPD ()
writeEnvTVar accessor val = MPD $ do
  tVar <- asks accessor
  liftIO . atomically $ writeTVar tVar val

-- | A response is either an 'MPDError' or some result.
type Response = Either MPDError

clearHandle :: MPD ()
clearHandle = MPD $ do
  tmHandle <- envHandle <$> ask
  liftIO . atomically $ writeTVar tmHandle Nothing

-- | The most configurable API for running an MPD action.
withMPDEx :: Host -> Port -> Password -> MPD a -> IO (Response a)
withMPDEx host port pw x = withSocketsDo $ do
    env <- initEnv
    runReaderT (runErrorT . runMPD $ open >> (x <* close)) env
    where
    initEnv = do
      tHandle <- newTVarIO Nothing
      tPassword <- newTVarIO ""
      tVersion <- newTVarIO (0, 0, 0)
      return $ MPDEnv host port tHandle tPassword tVersion

mpdOpen :: MPD ()
mpdOpen = MPD $ do
    env <- ask
    let host = envHost env
        port = envPort env
        tmHandle = envHandle env
    runMPD close
    mHandle <- liftIO (safeConnectTo host port)
    liftIO . atomically $ writeTVar tmHandle mHandle
    F.forM_ mHandle $ \_ -> runMPD checkConn >>= (`unless` runMPD close)
    where
        safeConnectTo host@('/':_) _ =
            (Just <$> connectTo "" (UnixSocket host))
            `catchAny` const (return Nothing)
        safeConnectTo host port =
            (Just <$> connectTo host (PortNumber $ fromInteger port))
            `catchAny` const (return Nothing)
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
                tVersion <- asks envVersion
                liftIO . atomically $ writeTVar tVersion version
                return True
            where
                requiredVersion = (0, 15, 0)

        parseVersion = parseTriple '.' parseNum . dropWhile (not . isDigit)

        formatVersion :: (Int, Int, Int) -> String
        formatVersion (x, y, z) = printf "%d.%d.%d" x y z


mpdClose :: MPD ()
mpdClose =
    readEnvTVar envHandle >>= F.mapM_ (\h -> do
        writeEnvTVar envHandle Nothing
        r <- liftIO $ sendClose h
        MPD $ F.forM_ r throwError
    )
    where
        sendClose handle =
            (hPutStrLn handle "close" >> hReady handle >> hClose handle >> return Nothing)
            `catch` handler

        handler err
            | isEOFError err = return Nothing
            | otherwise      = (return . Just . ConnectionError) err

mpdSend :: String -> MPD [ByteString]
mpdSend str = send' `catchError` handler
    where
        handler err
          | ConnectionError e <- err, isRetryable e = mpdOpen >> send'
          | otherwise = throwError err

        send' :: MPD [ByteString]
        send' = maybe (throwError NoMPD) go =<< readEnvTVar envHandle

        go handle = (liftIO . tryIOError $ do
            unless (null str) $ B.hPutStrLn handle (UTF8.fromString str) >> hFlush handle
            getLines handle [])
                >>= either (\err -> clearHandle
                                 >> throwError (ConnectionError err)) return

        getLines :: Handle -> [ByteString] -> IO [ByteString]
        getLines handle acc = do
            l <- B.hGetLine handle
            if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                then (return . reverse) (l:acc)
                else getLines handle (l:acc)

-- | Re-connect and retry for these Exceptions.
isRetryable :: E.IOException -> Bool
isRetryable e = or [ isEOFError e, isResourceVanished e ]

-- | Predicate to identify ResourceVanished exceptions.
-- Note: these are GHC only!
isResourceVanished :: GE.IOException -> Bool
isResourceVanished e = ioeGetErrorType e == GE.ResourceVanished

--
-- Other operations.
--

-- | Kill the server. Obviously, the connection is then invalid.
kill :: (MonadMPD m) => m ()
kill = send "kill" >> return ()

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
