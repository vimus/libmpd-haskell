{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.ClientToClient
Copyright   : (c) Joachim Fasting 2013
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Client to client communication.
-}

module Network.MPD.Applicative.ClientToClient
    ( -- * Types
      ChannelName
    , MessageText
      -- * Subscribing to channels
    , subscribe
    , unsubscribe
    , channels
      -- * Communicating with other clients
    , readMessages
    , sendMessage
    ) where

import           Control.Applicative

import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Applicative.Internal
import           Network.MPD.Applicative.Util
import           Network.MPD.Util

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

------------------------------------------------------------------------

type ChannelName = String
type MessageText = String

------------------------------------------------------------------------

subscribe :: ChannelName -> Command ()
subscribe name = Command emptyResponse ["subscribe" <@> name]

unsubscribe :: ChannelName -> Command ()
unsubscribe name = Command emptyResponse ["unsubscribe" <@> name]

channels :: Command [ChannelName]
channels = Command p ["channels"]
  where
    p = map UTF8.toString . takeValues <$> getResponse

------------------------------------------------------------------------

readMessages :: Command [(ChannelName, MessageText)]
readMessages = Command (liftParser p) ["readmessages"]
  where
    p = mapM parseMessage . splitGroups ["channel"] . toAssocList

    parseMessage :: [(ByteString, ByteString)] -> Either String (ChannelName, MessageText)
    parseMessage [("channel", ch),("message", msg)] = Right (UTF8.toString ch, UTF8.toString msg)
    parseMessage _ = Left "Unexpected result from readMessages"

sendMessage :: ChannelName -> MessageText -> Command ()
sendMessage name text = Command emptyResponse ["sendmessage" <@> name <++> text]
