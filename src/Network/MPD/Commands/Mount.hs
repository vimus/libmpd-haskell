{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Mount
Copyright   : (c) Joachim Fasting 2014
License     : MIT

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

Mounting remote storage.
-}

module Network.MPD.Commands.Mount
  ( mount
  , unmount
  , listMounts
  , listNeighbors
  ) where

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.Mount as A
import           Network.MPD.Core

mount :: (MonadMPD m) => String -> String -> m ()
mount p = A.runCommand . A.mount p

unmount :: (MonadMPD m) => String -> m ()
unmount = A.runCommand . A.unmount

listMounts :: (MonadMPD m) => m [(String, String)]
listMounts = A.runCommand A.listMounts

listNeighbors :: (MonadMPD m) => m [(String, String)]
listNeighbors = A.runCommand A.listNeighbors
