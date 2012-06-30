{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Connection
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Connection settings.
-}

module Network.MPD.Applicative.Connection
    ( password
    , ping
    ) where

import           Network.MPD.Applicative
import           Network.MPD.Core

-- | Authenticate session. The password is sent in plain text.
password :: Password -> Command ()
password pwd = Command emptyResponse ["password " ++ pwd]

-- | Ping daemon.
ping :: Command ()
ping = Command emptyResponse ["ping"]
