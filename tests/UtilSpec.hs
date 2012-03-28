{-# LANGUAGE OverloadedStrings #-}

module UtilSpec (main, spec) where

import           TestUtil
import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()

import           Network.MPD.Util

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "splitGroups" $ do
    it "breaks an association list into sublists" $ do
      splitGroups ["1", "5"]
        [("1","a"),("2","b"),
         ("5","c"),("6","d"),
         ("1","z"),("2","y"),("3","x")]
         `shouldBe`
         [[("1","a"),("2","b")],
         [("5","c"),("6","d")],
         [("1","z"),("2","y"),("3","x")]]
