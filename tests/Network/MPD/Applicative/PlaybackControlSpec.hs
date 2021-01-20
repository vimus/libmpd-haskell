{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.PlaybackControlSpec (main, spec) where

import           TestUtil

import           Network.MPD.Applicative.PlaybackControl
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "next" $ do
        it "sends a next request" $ do
            next `with` [("next", Right "OK")] `shouldBe` Right ()

    describe "toggle" $ do
        it "toggles playback" $ do
            toggle `with` [("pause", Right "OK")] `shouldBe` Right ()

    describe "play" $ do
        it "sends a play request" $ do
            play Nothing `with` [("play", Right "OK")] `shouldBe` Right ()
        
        it "optionally takes a position to start playback at" $ do
            play (Just 1) `with` [("play 1", Right "OK")] `shouldBe` Right ()

    describe "playId" $ do
        it "like 'play' but takes an id instead" $ do
            playId (Id 1) `with` [("playid 1", Right "OK")] `shouldBe` Right ()

    describe "previous" $ do
        it "sends a request" $ do
            previous `with` [("previous", Right "OK")] `shouldBe` Right ()

    describe "seek" $ do
        it "sends a seek request" $ do
            seek 1 10 `with` [("seek 1 10.0", Right "OK")] `shouldBe` Right ()

    describe "seekId" $ do
        it "is like 'seek' but takes an id" $ do
            seekId (Id 1) 10
                `with` [("seekid 1 10.0", Right "OK")]
                `shouldBe` Right ()
    describe "seekCur" $ do
        it "sends a seek request on the current song, absolute time" $ do
            seekCur True 10
                `with` [("seekcur 10.0", Right "OK")]
                `shouldBe` Right ()
        it "sends a seek request on the current song, positive relative time" $ do
            seekCur False 10
                `with` [("seekcur +10.0", Right "OK")]
                `shouldBe` Right ()
        it "sends a seek request on the current song, positive negative time" $ do
            seekCur False (-10)
                `with` [("seekcur -10.0", Right "OK")]
                `shouldBe` Right ()

    describe "stop" $ do
        it "sends a stop request" $ do
            stop `with` [("stop", Right "OK")] `shouldBe` Right ()
