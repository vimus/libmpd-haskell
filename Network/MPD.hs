-- | Module    : Network.MPD
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- An MPD client library. MPD is a daemon for playing music that is
-- controlled over a network socket. Its site is at <http://www.musicpd.org/>.
--
-- To avoid name clashes with the standard Prelude functions, do:
--
-- > import qualified Network.MPD as MPD

module Network.MPD (
    -- * Basic data types
    MPD, MPDError(..), ACKType(..), Response,
    -- * Connections
    withMPD, withMPDEx,
    module Network.MPD.Commands,
    ) where

import Network.MPD.Commands
import Network.MPD.Core
import Network.MPD.SocketConn

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Data.IORef (newIORef, atomicModifyIORef)
import System.Environment (getEnv)
import System.IO
import System.IO.Error (isDoesNotExistError, ioError)

-- | A wrapper for 'withMPDEx' that uses localhost:6600 as the default
-- host:port, or whatever is found in the environment variables MPD_HOST and
-- MPD_PORT. If MPD_HOST is of the form \"password\@host\" the password
-- will be supplied as well.
--
-- Examples:
--
-- > withMPD $ play Nothing
-- > withMPD $ add_ "" "tool" >> play Nothing >> currentSong
withMPD :: MPD a -> IO (Response a)
withMPD m = do
    port <- liftM read (getEnvDefault "MPD_PORT" "6600")
    (pw,host) <- liftM (break (== '@')) (getEnvDefault "MPD_HOST" "localhost")
    let (host',pw') = if null host then (pw,host) else (drop 1 host,pw)
    pwGen <- mkPasswordGen [pw']
    withMPDEx host' port pwGen m
    where
        getEnvDefault x dflt =
            catch (getEnv x) (\e -> if isDoesNotExistError e
                                    then return dflt else ioError e)

-- | Create an action that produces passwords for a connection. You
-- can pass these to 'withMPDEx' and it will use them to get passwords
-- to send to the server until one works or it runs out of them.
--
-- > do gen <- mkPasswordGen ["password1", "password2"]
-- >    withMPDEx "localhost" 6600 gen (update [])
mkPasswordGen :: [String] -> IO (IO (Maybe String))
mkPasswordGen = liftM f . newIORef
    where f = flip atomicModifyIORef $ \xs -> (drop 1 xs, listToMaybe xs)
