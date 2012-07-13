{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.ApplicativeSpec (main, spec) where

import           TestUtil

import           Data.List (intercalate)
import           Network.MPD.Applicative
import           Network.MPD.Commands.Types

import           Control.Applicative
import qualified Data.Map as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Command as an Applicative" $ do
    describe "currentSong" $ do
      it "returns the currently played song" $ do
        let response = songResponse ++ "list_OK\nOK\n"
        testMPD [("currentsong", Right response)] (runCommand currentSong) `shouldBe` Right (Just songValue)

    it "can be composed" $ do
      let command = intercalate "\n" [
              "command_list_ok_begin"
            , "currentsong"
            , "stats"
            , "command_list_end"
            ]
          response = songResponse ++ "list_OK\n" ++ statsResponse ++ "list_OK\nOK\n"
      let action = runCommand $ (,) <$> currentSong <*> stats
      testMPD [(command, Right response)] action `shouldBe` Right (Just songValue, statsValue)

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
