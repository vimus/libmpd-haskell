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
    withMPD, withMPDEx,
    module Network.MPD.Commands,
    ) where

import Network.MPD.Commands
import Network.MPD.Core
import Network.MPD.Utils

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
withMPD m = do
    port       <- read `liftM` getEnvDefault "MPD_PORT" "6600"
    (host, pw) <- parseHost `liftM` getEnvDefault "MPD_HOST" "localhost"
    withMPDEx host port pw m
    where
        getEnvDefault x dflt =
            catch (getEnv x) (\e -> if isDoesNotExistError e
                                    then return dflt else ioError e)
        parseHost s = case breakChar '@' s of
                          (host, "") -> (host, "")
                          (pw, host) -> (host, pw)
