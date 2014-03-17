{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      : Network.MPD.Core.Error
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
Stability   : unstable
Portability : unportable

MPD errors.
-}

module Network.MPD.Core.Error where

import qualified Control.Exception as E
import           Control.Monad.Error (Error(..))
import           Data.Typeable

-- | The MPDError type is used to signal errors, both from the MPD and
-- otherwise.
data MPDError = NoMPD              -- ^ MPD not responding
              | ConnectionError E.IOException -- ^ An error occurred while talking to MPD.
              | Unexpected String  -- ^ MPD returned an unexpected response.
                                   --   This is a bug, either in the library or
                                   --   in MPD itself.
              | Custom String      -- ^ Used for misc. errors
              | ACK ACKType String -- ^ ACK type and a message from the
                                   --   server
                deriving (Eq, Typeable)

instance E.Exception MPDError

instance Show MPDError where
    show NoMPD          = "Could not connect to MPD"
    show (ConnectionError e) = "Connection error (" ++ show e ++ ")"
    show (Unexpected s) = "MPD returned an unexpected response: " ++ unlines [
                            s
                          , ""
                          , "This is most likely a bug in libmpd! Please report it here:"
                          , ""
                          , "https://github.com/vimus/libmpd-haskell/issues/new"
                          ]
    show (Custom s)     = s
    show (ACK _ s)      = s

instance Error MPDError where
    noMsg  = Custom "An error occurred"
    strMsg = Custom

-- | Represents various MPD errors (aka. ACKs).
data ACKType = InvalidArgument  -- ^ Invalid argument passed (ACK 2)
             | InvalidPassword  -- ^ Invalid password supplied (ACK 3)
             | Auth             -- ^ Authentication required (ACK 4)
             | UnknownCommand   -- ^ Unknown command (ACK 5)
             | FileNotFound     -- ^ File or directory not found ACK 50)
             | PlaylistMax      -- ^ Playlist at maximum size (ACK 51)
             | System           -- ^ A system error (ACK 52)
             | PlaylistLoad     -- ^ Playlist loading failed (ACK 53)
             | Busy             -- ^ Update already running (ACK 54)
             | NotPlaying       -- ^ An operation requiring playback
                                --   got interrupted (ACK 55)
             | FileExists       -- ^ File already exists (ACK 56)
             | UnknownACK       -- ^ An unknown ACK (aka. bug)
               deriving Eq
