module Network.MPD.Applicative where

import           Control.Applicative
import           Network.MPD.Commands.Types
import           Network.MPD.Core

data Command a

instance Functor Command where
  fmap = undefined

instance Applicative Command where
  pure  = undefined
  (<*>) = undefined

currentSong :: Command (Maybe Song)
currentSong = undefined

stats :: Command Stats
stats = undefined

runCommand :: MonadMPD m => Command a -> m a
runCommand = undefined
