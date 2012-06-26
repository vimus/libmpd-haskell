{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Stickers
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Stickers.
-}

module Network.MPD.Commands.Stickers
    ( stickerGet
    , stickerSet
    , stickerDelete
    , stickerList
    , stickerFind
    ) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util
import           Network.MPD.Core
import           Network.MPD.Util

import           Control.Monad (liftM)

import qualified Data.ByteString.UTF8 as UTF8

-- | Reads a sticker value for the specified object.
stickerGet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> m [String]
stickerGet typ uri name = (map UTF8.toString . takeValues) `liftM` getResponse ("sticker get" <@> typ <++> uri <++> name)

-- | Adds a sticker value to the specified object.
stickerSet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> String -- ^ Sticker value
           -> m ()
stickerSet typ uri name value =
    getResponse_ ("sticker set" <@> typ <++> uri <++> name <++> value)

-- | Delete a sticker value from the specified object.
stickerDelete :: MonadMPD m => ObjectType
              -> String -- ^ Object URI
              -> String -- ^ Sticker name
              -> m ()
stickerDelete typ uri name =
    getResponse_ ("sticker delete" <@> typ <++> uri <++> name)


-- | Lists the stickers for the specified object.
stickerList :: MonadMPD m => ObjectType
            -> String -- ^ Object URI
            -> m [(String, String)] -- ^ Sticker name\/sticker value
stickerList typ uri =
    (map decodePair . toAssocList) `liftM` getResponse ("sticker list" <@> typ <++> uri)

-- | Searches the sticker database for stickers with the specified name, below
-- the specified path.
stickerFind :: MonadMPD m => ObjectType
            -> String -- ^ Path
            -> String -- ^ Sticker name
            -> m [(String, String)] -- ^ URI\/sticker value
stickerFind typ uri name =
    (map decodePair . toAssocList) `liftM`
    getResponse ("sticker find" <@> typ <++> uri <++> name)
