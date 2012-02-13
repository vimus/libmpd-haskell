{-# OPTIONS_GHC -Wwarn -fno-warn-missing-signatures #-}
-- |
-- GUTD: The Grand Unified Test Driver.
import qualified Commands
import qualified Properties
import qualified Spec
import           Test.Hspec.Monadic (hspecX)

main = do
    putStrLn "*** Properties ***"
    Properties.main

    putStrLn "\n*** Unit Tests ***"
    Commands.main

    putStrLn "\n*** Specs ***"
    hspecX Spec.spec
