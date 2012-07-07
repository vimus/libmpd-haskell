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
  where
    -- Builds a 'Status' instance from an assoc. list.
    parseStatus :: [ByteString] -> Either String Status
    parseStatus = foldM f def . toAssocList
        where f a ("state", x)
                  = return $ parse state     (\x' -> a { stState = x' }) a x
              f a ("volume", x)
                  = return $ parse parseNum  (\x' -> a { stVolume = x' }) a x
              f a ("repeat", x)
                  = return $ parse parseBool (\x' -> a { stRepeat = x' }) a x
              f a ("random", x)
                  = return $ parse parseBool (\x' -> a { stRandom = x' }) a x
              f a ("playlist", x)
                  = return $ parse parseNum  (\x' -> a { stPlaylistVersion = x' }) a x
              f a ("playlistlength", x)
                  = return $ parse parseNum  (\x' -> a { stPlaylistLength = x' }) a x
              f a ("xfade", x)
                  = return $ parse parseNum  (\x' -> a { stXFadeWidth = x' }) a x
              f a ("mixrampdb", x)
                  = return $ parse parseFrac (\x' -> a { stMixRampdB = x' }) a x
              f a ("mixrampdelay", x)
                  = return $ parse parseFrac (\x' -> a { stMixRampDelay = x' }) a x
              f a ("song", x)
                  = return $ parse parseNum  (\x' -> a { stSongPos = Just x' }) a x
              f a ("songid", x)
                  = return $ parse parseNum  (\x' -> a { stSongID = Just $ Id x' }) a x
              f a ("time", x)
                  = return $ parse time      (\x' -> a { stTime = x' }) a x
              f a ("elapsed", x)
                  = return $ parse parseFrac (\x' -> a { stTime = (x', snd $ stTime a) }) a x
              f a ("bitrate", x)
                  = return $ parse parseNum  (\x' -> a { stBitrate = x' }) a x
              f a ("audio", x)
                  = return $ parse audio     (\x' -> a { stAudio = x' }) a x
              f a ("updating_db", x)
                  = return $ parse parseNum  (\x' -> a { stUpdatingDb = Just x' }) a x
              f a ("error", x)
                  = return a { stError = Just (UTF8.toString x) }
              f a ("single", x)
                  = return $ parse parseBool (\x' -> a { stSingle = x' }) a x
              f a ("consume", x)
                  = return $ parse parseBool (\x' -> a { stConsume = x' }) a x
              f a ("nextsong", x)
                  = return $ parse parseNum  (\x' -> a { stNextSongPos = Just x' }) a x
              f a ("nextsongid", x)
                  = return $ parse parseNum  (\x' -> a { stNextSongID = Just $ Id x' }) a x
              f _ x
                  = fail $ show x

              state "play"  = Just Playing
              state "pause" = Just Paused
              state "stop"  = Just Stopped
              state _       = Nothing

              time s = case parseFrac *** parseNum $ breakChar ':' s of
                           (Just a, Just b) -> Just (a, b)
                           _                -> Nothing

              audio = parseTriple ':' parseNum
