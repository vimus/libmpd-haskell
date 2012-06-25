{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Status
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Querying MPD's status.
-}

module Network.MPD.Commands.Status
    ( clearError
    , currentSong
    , idle
    , noidle
    , stats
    , status
    ) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util
import           Network.MPD.Core
import           Network.MPD.Util

import           Control.Monad (liftM)
import           Prelude hiding (repeat, read)

import qualified Data.ByteString.UTF8 as UTF8

-- | Clear the current error message in status.
clearError :: MonadMPD m => m ()
clearError = getResponse_ "clearerror"

-- | Get the currently playing song.
currentSong :: (Functor m, MonadMPD m) => m (Maybe Song)
currentSong = getResponse "currentsong" >>= runParser parseMaybeSong . toAssocList

-- | Wait until there is a noteworthy change in one or more of MPD's
-- susbystems.
--
-- The first argument is a list of subsystems that should be considered.  An
-- empty list specifies that all subsystems should be considered.
--
-- A list of subsystems that have noteworthy changes is returned.
--
-- Note that running this command will block until either 'idle' returns or is
-- cancelled by 'noidle'.
idle :: MonadMPD m => [Subsystem] -> m [Subsystem]
idle subsystems =
    mapM f =<< toAssocList `liftM` getResponse ("idle" <@> foldr (<++>) (Args []) subsystems)
    where
        f ("changed", system) =
            case system of
                "database"        -> return DatabaseS
                "update"          -> return UpdateS
                "stored_playlist" -> return StoredPlaylistS
                "playlist"        -> return PlaylistS
                "player"          -> return PlayerS
                "mixer"           -> return MixerS
                "output"          -> return OutputS
                "options"         -> return OptionsS
                k                 -> fail ("Unknown subsystem: " ++ UTF8.toString k)
        f x                       =  fail ("idle: Unexpected " ++ show x)

-- | Cancel 'idle'.
noidle :: MonadMPD m => m ()
noidle = getResponse_ "noidle"

-- | Get server statistics.
stats :: MonadMPD m => m Stats
stats = getResponse "stats" >>= runParser parseStats

-- | Get the server's status.
status :: MonadMPD m => m Status
status = getResponse "status" >>= runParser parseStatus
