{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Status
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : MIT (see LICENSE)

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

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.Status as A
import           Network.MPD.Commands.Types
import           Network.MPD.Core

-- | Clear the current error message in status.
clearError :: MonadMPD m => m ()
clearError = A.runCommand A.clearError

-- | Get the currently playing song.
currentSong :: MonadMPD m => m (Maybe Song)
currentSong = A.runCommand A.currentSong

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
idle = A.runCommand . A.idle

-- | Cancel 'idle'.
noidle :: MonadMPD m => m ()
noidle = A.runCommand A.noidle

-- | Get server statistics.
stats :: MonadMPD m => m Stats
stats = A.runCommand A.stats

-- | Get the server's status.
status :: MonadMPD m => m Status
status = A.runCommand A.status
