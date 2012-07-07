{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.StatusSpec (main, spec) where

import           TestUtil
import           Unparse

import           Data.Default

import           Network.MPD.Applicative.Status
import           Network.MPD.Commands.Types
import           Network.MPD.Core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- XXX: generalize to arbitrary Stats
  describe "stats" $ do
    it "returns database statistics" $ do
      let obj = def { stsArtists = 1, stsAlbums = 1, stsSongs =  1
                    , stsUptime = 100, stsPlaytime = 100, stsDbUpdate = 10
                    , stsDbPlaytime = 100 }
          resp = unparse obj ++ "OK"
      stats `with` [("stats", Right resp)] `shouldBe` Right obj

  -- XXX: generalize to arbitrary Status
  describe "status" $ do
    it "returns daemon status" $ do
      let obj = def :: Status
          resp = unparse obj ++ "OK"
      status `with` [("status", Right resp)] `shouldBe` Right obj

    it "fails on unexpected key-value pairs" $ do
      let resp = unparse (def :: Status) ++ unlines [
              "foo: bar"
            , "OK\n"
            ]
      status `with` [("status", Right resp)] `shouldBe` Left (Unexpected $ "unexpected key-value pair: (\"foo\",\"bar\")")

    it "fails on unexpected value" $ do
      let resp = unlines [
              "volume: foo"
            , "OK\n"
            ]
      status `with` [("status", Right resp)] `shouldBe` Left (Unexpected $ "unexpected key-value pair: (\"volume\",\"foo\")")

  describe "clearError" $ do
    it "sends a clearerror request" $ do
      clearError `with` [("clearerror", Right "OK")] `shouldBe` Right ()

  -- XXX: generalize to arbitrary Subsystem
  describe "idle" $ do
    it "sends an idle request" $ do
      idle [DatabaseS]
        `with` [("idle database", Right "changed: database\nOK")]
        `shouldBe` Right [DatabaseS]

  describe "noidle" $ do
    it "sends a noidle request" $ do
      noidle `with` [("noidle", Right "OK")] `shouldBe` Right ()
