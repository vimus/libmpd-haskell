module TestUtil (shouldBe) where

import           Test.Hspec.HUnit ()
import           Test.HUnit

shouldBe :: (Eq a, Show a) => a -> a -> Assertion
shouldBe = (@?=)
