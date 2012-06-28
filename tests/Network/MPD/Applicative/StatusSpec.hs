{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.StatusSpec (main, spec) where

import           Defaults ()
import           TestUtil
import           Unparse

import           Data.Default

import           Network.MPD.Applicative
import           Network.MPD.Applicative.Status
import           Network.MPD.Commands.Types

import           Control.Applicative
import qualified Data.Map as M

songResponse :: String
songResponse = unlines [
    "file: Trip-Hop/Morcheeba/Morcheeba - 2010 - Blood Like Lemonade/03 - Blood Like Lemonade.mp3"
  , "Last-Modified: 2010-08-01T11:37:50Z"
  , "Time: 291"
  , "Artist: Morcheeba"
  , "Title: Blood Like Lemonade"
  , "Album: Blood Like Lemonade"
  , "Track: 3"
  , "Date: 2010"
  , "Pos: 16"
  , "Id: 80"
  ]

songValue :: Song
songValue = Song {
    sgFilePath      = "Trip-Hop/Morcheeba/Morcheeba - 2010 - Blood Like Lemonade/03 - Blood Like Lemonade.mp3"
  , sgTags          = M.fromList [
                          (Artist,[Value "Morcheeba"])
                        , (Album,[Value "Blood Like Lemonade"])
                        , (Title,[Value "Blood Like Lemonade"])
                        , (Track,[Value "3"])
                        , (Date,[Value "2010"])
                        ]
  , sgLastModified  = Just (read "2010-08-01 11:37:50 UTC")
  , sgLength        = 291
  , sgId            = Just (Id 80)
  , sgIndex         = Just 16
  }

statsResponse :: String
statsResponse = unlines [
    "artists: 23"
  , "albums: 42"
  , "songs: 65"
  , "uptime: 120"
  , "playtime: 240"
  , "db_playtime: 560"
  , "db_update: 1024"
  ]

statsValue :: Stats
statsValue = Stats {
    stsArtists    = 23
  , stsAlbums     = 42
  , stsSongs      = 65
  , stsUptime     = 120
  , stsPlaytime   = 240
  , stsDbPlaytime = 560
  , stsDbUpdate   = 1024
  }

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Command as an Applicative" $ do
    describe "currentSong" $ do
      it "returns the currently played song" $ do
        let response = songResponse ++ "list_OK\nOK\n"
        testMPD [("currentsong", Right response)] (runCommand currentSong) `shouldBe` Right (Just songValue)

    -- XXX: move this to ApplicativeSpec
    it "can be composed" $ do
      let command = unlines [
              "command_list_ok_begin"
            , "currentsong"
            , "stats"
            , "command_list_end"
            ]
          response = songResponse ++ "list_OK\n" ++ statsResponse ++ "list_OK\nOK\n"
      let action = runCommand $ (,) <$> currentSong <*> stats
      testMPD [(command, Right response)] action `shouldBe` Right (Just songValue, statsValue)

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
