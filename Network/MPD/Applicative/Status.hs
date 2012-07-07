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
import           Control.Monad
import           Control.Arrow ((***))

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
                "database"        -> Right DatabaseS
                "update"          -> Right UpdateS
                "stored_playlist" -> Right StoredPlaylistS
                "playlist"        -> Right PlaylistS
                "player"          -> Right PlayerS
                "mixer"           -> Right MixerS
                "output"          -> Right OutputS
                "options"         -> Right OptionsS
                k                 -> Left ("Unknown subsystem: " ++ UTF8.toString k)
        f x                       =  Left ("idle: Unexpected " ++ show x)

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
  where
    -- Builds a 'Status' instance from an assoc. list.
    parseStatus :: [ByteString] -> Either String Status
    parseStatus = foldM go def . toAssocList
        where
            go a p@(k, v) = case k of
                "volume"         -> num   $ \x -> a { stVolume          = x }
                "repeat"         -> bool  $ \x -> a { stRepeat          = x }
                "random"         -> bool  $ \x -> a { stRandom          = x }
                "single"         -> bool  $ \x -> a { stSingle          = x }
                "consume"        -> bool  $ \x -> a { stConsume         = x }
                "playlist"       -> num   $ \x -> a { stPlaylistVersion = x }
                "playlistlength" -> num   $ \x -> a { stPlaylistLength  = x }
                "state"          -> state $ \x -> a { stState           = x}
                "song"           -> num   $ \x -> a { stSongPos         = Just x }
                "songid"         -> num   $ \x -> a { stSongID          = Just $ Id x }
                "nextsong"       -> num   $ \x -> a { stNextSongPos     = Just x }
                "nextsongid"     -> num   $ \x -> a { stNextSongID      = Just $ Id x }
                "time"           -> time  $ \x -> a { stTime            = x }
                "elapsed"        -> frac  $ \x -> a { stTime            = (x, snd $ stTime a) }
                "bitrate"        -> num   $ \x -> a { stBitrate         = x }
                "xfade"          -> num   $ \x -> a { stXFadeWidth      = x }
                "mixrampdb"      -> frac  $ \x -> a { stMixRampdB       = x }
                "mixrampdelay"   -> frac  $ \x -> a { stMixRampDelay    = x }
                "audio"          -> audio $ \x -> a { stAudio           = x }
                "updating_db"    -> num   $ \x -> a { stUpdatingDb      = Just x }
                "error"          -> Right         a { stError           = Just (UTF8.toString v) }
                _                -> unexpectedPair
                where
                    unexpectedPair = Left ("unexpected key-value pair: " ++ show p)
                    num   f = maybe unexpectedPair (Right . f) (parseNum  v)
                    bool  f = maybe unexpectedPair (Right . f) (parseBool v)
                    frac  f = maybe unexpectedPair (Right . f) (parseFrac v)
                    audio f = maybe unexpectedPair (Right . f) (parseTriple ':' parseNum v)

                    time f = case parseFrac *** parseNum $ breakChar ':' v of
                                 (Just a_, Just b) -> (Right . f) (a_, b)
                                 _                 -> unexpectedPair

                    state f = case v of
                        "play"  -> (Right . f) Playing
                        "pause" -> (Right . f) Paused
                        "stop"  -> (Right . f) Stopped
                        _       -> unexpectedPair
