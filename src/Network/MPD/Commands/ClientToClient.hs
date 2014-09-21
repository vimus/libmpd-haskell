{-|
Module      : Network.MPD.Commands.ClientToClient
Copyright   : (c) Joachim Fasting 2013
License     : MIT (see LICENSE)

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

Client-to-client communication.
-}

module Network.MPD.Commands.ClientToClient
    ( -- * Types
      A.ChannelName
    , A.MessageText
      -- * Subscribing to channels
    , subscribe
    , unsubscribe
    , channels
      -- * Communicating with other clients
    , readMessages
    , sendMessage
    ) where

------------------------------------------------------------------------

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.ClientToClient as A
import           Network.MPD.Core

------------------------------------------------------------------------

subscribe :: MonadMPD m => A.ChannelName -> m ()
subscribe = A.runCommand . A.subscribe

unsubscribe :: MonadMPD m => A.ChannelName -> m ()
unsubscribe = A.runCommand . A.subscribe

channels :: MonadMPD m => m [A.ChannelName]
channels = A.runCommand A.channels

readMessages :: MonadMPD m => m [(A.ChannelName, A.MessageText)]
readMessages = A.runCommand A.readMessages

sendMessage :: MonadMPD m => A.ChannelName -> A.MessageText -> m ()
sendMessage name text = A.runCommand (A.sendMessage name text)
