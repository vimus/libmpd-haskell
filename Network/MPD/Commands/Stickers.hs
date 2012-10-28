{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Stickers
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : MIT (see LICENSE)

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

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.Stickers as A
import           Network.MPD.Commands.Types
import           Network.MPD.Core

-- | Reads a sticker value for the specified object.
stickerGet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> m [String]
stickerGet typ uri = A.runCommand . A.stickerGet typ uri

-- | Adds a sticker value to the specified object.
stickerSet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> String -- ^ Sticker value
           -> m ()
stickerSet typ uri name = A.runCommand . A.stickerSet typ uri name

-- | Delete a sticker value from the specified object.
stickerDelete :: MonadMPD m => ObjectType
              -> String -- ^ Object URI
              -> String -- ^ Sticker name
              -> m ()
stickerDelete typ uri = A.runCommand . A.stickerDelete typ uri

-- | Lists the stickers for the specified object.
stickerList :: MonadMPD m => ObjectType
            -> String -- ^ Object URI
            -> m [(String, String)] -- ^ Sticker name\/sticker value
stickerList typ = A.runCommand . A.stickerList typ

-- | Searches the sticker database for stickers with the specified name, below
-- the specified path.
stickerFind :: MonadMPD m => ObjectType
            -> String -- ^ Path
            -> String -- ^ Sticker name
            -> m [(String, String)] -- ^ URI\/sticker value
stickerFind typ uri = A.runCommand . A.stickerFind typ uri
