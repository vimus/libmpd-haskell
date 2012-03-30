module Main (main) where

import qualified CommandSpec
import qualified EnvSpec
import qualified ParserSpec
import qualified UtilSpec
import           Test.Hspec.Monadic (describe, hspecX)

main :: IO ()
main = hspecX $ do
    describe "CommandSpec" CommandSpec.spec
    describe "EnvSpec" EnvSpec.spec
    describe "ParserSpec" ParserSpec.spec
    describe "UtilSpec" UtilSpec.spec
