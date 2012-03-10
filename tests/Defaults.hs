{-# OPTIONS_GHC -fno-warn-orphans #-}

module Defaults where

import           Data.Default
import           Network.MPD.Commands.Types

instance Default Count where
    def = defaultCount

instance Default Device where
    def = defaultDevice

-- XXX: note Song has no sensible default value
-- XXX: or does it?

instance Default Stats where
    def = defaultStats

instance Default Status where
    def = defaultStatus
