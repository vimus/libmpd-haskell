-- | Module    : Network.MPD
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- An MPD client library. MPD is a daemon for playing music that is
-- controlled over a network socket. Its site is at <http://www.musicpd.org/>.
--
-- To avoid name clashes with the standard Prelude functions, do:
--
-- > import qualified Network.MPD as MPD

module Network.MPD (
    -- * Basic data types
    MonadMPD, MPD, MPDError(..), ACKType(..), Response,
    Host, Port, Password,
    -- * Connections
    withMPD, withMPD_, withMPDEx,
    module Network.MPD.Commands,
    ) where

import Prelude hiding (catch)
import Control.Exception
import Network.MPD.Commands
import Network.MPD.Core
import Network.MPD.Util

import Control.Monad (liftM)
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)

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
    (host, port, pw) <- getConnectionSettings mHost mPort
    withMPDEx host port pw action

getConnectionSettings :: Maybe String -> Maybe String -> IO (Host, Port, Password)
getConnectionSettings mHost mPort = do
    (host, pw) <- parseHost `fmap`
        maybe (getEnvDefault "MPD_HOST" "localhost") return mHost
    port <- read `fmap`
        maybe (getEnvDefault "MPD_PORT" "6600") return mPort
    return (host, port, pw)
    where
        getEnvDefault x dflt =
            catch (getEnv x) (\e -> if isDoesNotExistError e
                                    then return dflt else ioError e)
        parseHost s = case breakChar '@' s of
                          (host, "") -> (host, "")
                          (pw, host) -> (host, pw)
