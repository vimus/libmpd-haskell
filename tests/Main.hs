{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- |
-- GUTD: The Grand Unified Test Driver.
import qualified Commands
import qualified Properties

main = do
    putStrLn "*** Properties ***"
    Properties.main

    putStrLn "\n*** Unit Tests ***"
    Commands.main
