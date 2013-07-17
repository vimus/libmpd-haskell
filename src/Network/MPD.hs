{-# LANGUAGE CPP #-}

{- |
Module      : Network.MPD
Copyright   : (c) Joachim Fasting, Simon Hengel 2012
License     : MIT

Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
Stability   : unstable
Portability : unportable

An MPD client library. MPD is a daemon for playing music that is
controlled over a network socket.

To use the library, do:

> {-# LANGUAGE OverloadedStrings #-}
> import qualified Network.MPD as MPD
-}

module Network.MPD (
    -- * Basic data types
    MonadMPD, MPD, MPDError(..), ACKType(..), Response,
    Host, Port, Password,
    -- * Connections
    withMPD, withMPD_, withMPDEx,
    module Network.MPD.Commands,
#ifdef TEST
    getConnectionSettings, getEnvDefault
#endif
    ) where

import           Prelude
import qualified Control.Exception as E
import           Network.MPD.Commands
import           Network.MPD.Core

import           System.Environment (getEnv)
import           System.IO.Error (isDoesNotExistError)
import           Data.Maybe (listToMaybe)

-- | A wrapper for 'withMPDEx' that uses localhost:6600 as the default
-- host:port, or whatever is found in the environment variables MPD_HOST and
-- MPD_PORT. If MPD_HOST is of the form \"password\@host\" the password
-- will be supplied as well.
--
-- Examples:
--
-- > withMPD $ play Nothing
-- > withMPD $ add_ "tool" >> play Nothing >> currentSong
withMPD :: MPD a -> IO (Response a)
withMPD = withMPD_ Nothing Nothing

-- | Same as `withMPD`, but takes optional arguments that override MPD_HOST and
-- MPD_PORT.
--
-- This is e.g. useful for clients that optionally take @--port@ and @--host@
-- as command line arguments, and fall back to `withMPD`'s defaults if those
-- arguments are not given.
withMPD_ :: Maybe String -- ^ optional override for MPD_HOST
         -> Maybe String -- ^ optional override for MPD_PORT
         -> MPD a -> IO (Response a)
withMPD_ mHost mPort action = do
    settings <- getConnectionSettings mHost mPort
    case settings of
      Right (host, port, pw) -> withMPDEx host port pw action
      Left err -> (return . Left . Custom) err

getConnectionSettings :: Maybe String -> Maybe String -> IO (Either String (Host, Port, Password))
getConnectionSettings mHost mPort = do
    (host, pw) <- parseHost `fmap`
        maybe (getEnvDefault "MPD_HOST" "localhost") return mHost
    port <- maybe (getEnvDefault "MPD_PORT" "6600") return mPort
    case maybeRead port of
      Just p  -> (return . Right) (host, p, pw)
      Nothing -> (return . Left) (show port ++ " is not a valid port!")
    where
        parseHost s = case breakChar '@' s of
                          (host, "") -> (host, "")
                          (pw, host) -> (host, pw)

getEnvDefault :: String -> String -> IO String
getEnvDefault x dflt =
    E.catch (getEnv x) (\e -> if isDoesNotExistError e
                            then return dflt else ioError e)

-- Break a string by character, removing the separator.
breakChar :: Char -> String -> (String, String)
breakChar c s = let (x, y) = break (== c) s in (x, drop 1 y)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
