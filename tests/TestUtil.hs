module TestUtil (
  shouldBe
, with
, module StringConn
, module Test.Hspec.Monadic
) where

import           Network.MPD.Core
import           Network.MPD.Applicative

import           Test.Hspec.HUnit ()
import           Test.Hspec.Monadic
import           Test.HUnit
import           StringConn

shouldBe :: (Eq a, Show a) => a -> a -> Assertion
shouldBe = (@?=)

with :: Eq a => Command a -> [(Expect, Response String)] -> Response a
with = flip testMPD . runCommand
