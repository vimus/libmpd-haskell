{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Stickers
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Stickers.
-}

module Network.MPD.Applicative.Stickers
    ( stickerGet
    , stickerSet
    , stickerDelete
    , stickerList
    , stickerFind
    ) where

import           Network.MPD.Applicative.Internal
import           Network.MPD.Applicative.Util
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Types
import           Network.MPD.Util

import           Control.Applicative

import qualified Data.ByteString.UTF8 as UTF8

-- | Read sticker value for the object specified.
stickerGet :: ObjectType -> String -> String -> Command [String]
stickerGet typ uri name = Command p c
    where
        p :: Parser [String]
        p = map UTF8.toString . takeValues <$> getResponse

        c = ["sticker get" <@> typ <++> uri <++> name]

-- | Add sticker value to the object. Will overwrite existing stickers
-- with the same name.
stickerSet :: ObjectType -> String -> String -> String -> Command ()
stickerSet typ uri name value = Command emptyResponse c
    where
        c = ["sticker set" <@> typ <++> uri <++> name <++> value]

-- | Delete a sticker value from the object. If no sticker name is
-- given, all sticker values attached to the object are deleted.
stickerDelete :: ObjectType -> String -> String -> Command ()
stickerDelete typ uri name = Command emptyResponse c
    where
        c = ["sticker delete" <@> typ <++> uri <++> name]

-- | List stickers for the object.
stickerList :: ObjectType -> String -> Command [(String, String)]
stickerList typ uri = Command p c
    where
        p = map decodePair . toAssocList <$> getResponse

        c = ["sticker list" <@> typ <++> uri]

-- | Search the sticker database for stickers with the specified name,
-- below the specified directory.
stickerFind :: ObjectType -> String -> String -> Command [(String, String)]
stickerFind typ uri name = Command p c
    where
        p = map decodePair . toAssocList <$> getResponse

        c = ["sticker find" <@> typ <++> uri <++> name]
