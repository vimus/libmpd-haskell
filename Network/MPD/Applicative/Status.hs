{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Status
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Querying MPD's status.
-}

module Network.MPD.Applicative.Status
    ( clearError
    , currentSong
    , idle
    , noidle
    , status
    , stats
    ) where

import           Network.MPD.Util
import           Network.MPD.Applicative.Internal
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

-- | Clear current error message in status.
clearError :: Command ()
clearError = Command emptyResponse ["clearerror"]

-- | Song metadata for currently playing song, if any.
currentSong :: Command (Maybe Song)
currentSong = Command (liftParser parseMaybeSong) ["currentsong"]

takeSubsystems :: [ByteString] -> Either String [Subsystem]
takeSubsystems = mapM f . toAssocList
    where
        f :: (ByteString, ByteString) -> Either String Subsystem
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

-- | Wait until there is noteworthy change in one or more of MPD's
-- subsystems.
-- When active, only 'noidle' commands are allowed.
idle :: [Subsystem] -> Command [Subsystem]
idle ss = Command (liftParser takeSubsystems) c
    where
        c = ["idle" <@> foldr (<++>) (Args []) ss]

-- | Cancel an 'idle' request.
noidle :: Command ()
noidle = Command emptyResponse ["noidle"]

-- | Get database statistics.
stats :: Command Stats
stats = Command (liftParser parseStats) ["stats"]

-- | Get the current status of the player.
status :: Command Status
status = Command (liftParser parseStatus) ["status"]
