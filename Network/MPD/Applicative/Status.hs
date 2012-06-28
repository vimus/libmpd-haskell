{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.Status where

import           Network.MPD.Applicative
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types

currentSong :: Command (Maybe Song)
currentSong = Command (liftParser parseMaybeSong) ["currentsong"]

stats :: Command Stats
stats = Command (liftParser parseStats) ["stats"]
