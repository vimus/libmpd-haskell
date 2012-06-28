{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.PlaybackControl
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Controlling playback.
-}

module Network.MPD.Applicative.PlaybackControl
    ( next
    , pause
    , play
    , playId
    , previous
    , seek
    , seekId
    , stop
    ) where

import           Network.MPD.Applicative
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Types

-- | Play next song in the playlist.
next :: Command ()
next = Command emptyResponse ["next"]

-- | Toggle pause.
pause :: Bool -> Command ()
pause f = Command emptyResponse ["pause" <@> f]

-- | Begin playback (optionally at a specific position).
play :: Maybe Position -> Command ()
play mbPos = Command emptyResponse c
    where
        c = return $ maybe "play" ("play" <@>) mbPos

-- | Begin playback at the specified song id.
playId :: Id -> Command ()
playId id' = Command emptyResponse ["playid" <@> id']

-- | Play previous song.
previous :: Command ()
previous = Command emptyResponse ["previous"]

-- | Seek to time in the song at the given position.
seek :: Position -> Seconds -> Command ()
seek pos time = Command emptyResponse ["seek" <@> pos <++> time]

-- | Seek to time in the song with the given id.
seekId :: Id -> Seconds -> Command ()
seekId id' time = Command emptyResponse ["seekid" <@> id' <++> time]

-- | Stop playback.
stop :: Command ()
stop = Command emptyResponse ["stop"]
