{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.PlaybackControl
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

Controlling playback.
-}

module Network.MPD.Applicative.PlaybackControl
    ( next
    , pause
    , toggle
    , play
    , playId
    , previous
    , seek
    , seekId
    , seekCur
    , stop
    ) where

import           Network.MPD.Applicative.Internal
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Types

-- | Play next song in the playlist.
next :: Command ()
next = Command emptyResponse ["next"]

-- | Pauses playback on True, resumes on False.
pause :: Bool -> Command ()
pause f = Command emptyResponse ["pause" <@> f]

-- | Toggles playback.
--
-- @since 0.10.0.0
toggle :: Command ()
toggle = Command emptyResponse ["pause"]


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
seek :: Position -> FractionalSeconds -> Command ()
seek pos time = Command emptyResponse ["seek" <@> pos <++> time]

-- | Seek to time in the song with the given id.
seekId :: Id -> FractionalSeconds -> Command ()
seekId id' time = Command emptyResponse ["seekid" <@> id' <++> time]

-- | Seek to time in the current song. Absolute time for True in
-- the frist argument, relative time for False.
--
-- @since 0.9.2.0
seekCur :: Bool -> FractionalSeconds -> Command ()
seekCur bool time
  | bool      = Command emptyResponse ["seekcur" <@> time]
  | otherwise = Command emptyResponse ["seekcur" <@> (Sign time)]

-- | Stop playback.
stop :: Command ()
stop = Command emptyResponse ["stop"]
