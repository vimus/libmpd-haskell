{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Mount
Copyright   : (c) Joachim Fasting 2014
License     : MIT

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

Mounting remote storage.
-}

module Network.MPD.Applicative.Mount
  ( mount
  , unmount
  , listMounts
  , listNeighbors
  ) where

import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Applicative.Internal
import           Network.MPD.Util

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

mount :: String -- Path
      -> String -- Uri
      -> Command ()
mount p u = Command emptyResponse ["mount" <@> p <++> u]

unmount :: String -- Path
        -> Command ()
unmount p = Command emptyResponse ["unmount" <@> p]

listMounts :: Command [(String, String)] -- (Path, Uri)
listMounts = Command (liftParser p) ["listmounts"]
  where
    p = mapM parseMount . splitGroups ["mount"] . toAssocList
    parseMount :: [(ByteString, ByteString)] -> Either String (String, String)
    parseMount [("mount", mo), ("storage", st)] = Right (UTF8.toString mo, UTF8.toString st)
    parseMount _ = Left "Unexpected result from listMounts"

listNeighbors :: Command [(String, String)] -- (Uri, Name)
listNeighbors = Command (liftParser p) ["listneighbors"]
  where
    p = mapM parseNeighbor . splitGroups ["neighbor"] . toAssocList
    parseNeighbor :: [(ByteString, ByteString)] -> Either String (String, String)
    parseNeighbor [("neighbor", ne), ("name", na)] = Right (UTF8.toString ne, UTF8.toString na)
    parseNeighbor _ = Left "Unexpected result from listNeighbors"
