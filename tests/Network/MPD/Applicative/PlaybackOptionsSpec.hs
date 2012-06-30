{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.PlaybackOptionsSpec (main, spec) where

import           TestUtil

import           Network.MPD.Applicative.PlaybackOptions
import           Network.MPD.Commands.Types

import           Prelude hiding (repeat)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "consume" $ do
        it "can enable consume" $ do
            consume True `with` [("consume 1", Right "OK")]
                `shouldBe` Right ()

        it "can disable consume" $ do
            consume False `with` [("consume 0", Right "OK")]
                `shouldBe` Right ()

    describe "crossfade" $ do
        it "sets crossfade" $ do
            crossfade 1 `with` [("crossfade 1", Right "OK")]
                `shouldBe` Right ()

    describe "random" $ do
        it "can enable random" $ do
            random True `with` [("random 1", Right "OK")]
                `shouldBe` Right ()

        it "can disable random" $ do
            random False `with` [("random 0", Right "OK")]
                `shouldBe` Right ()

    describe "repeat" $ do
        it "can enable repeat" $ do
            repeat True `with` [("repeat 1", Right "OK")]
                `shouldBe` Right ()

        it "can disable repeat" $ do
            repeat False `with` [("repeat 0", Right "OK")]
                `shouldBe` Right ()

    describe "setVolume" $ do
        it "sets the volume" $ do
            setVolume 10 `with` [("setvol 10", Right "OK")]
                `shouldBe` Right ()

    describe "single" $ do
        it "can enable single" $ do
            single True `with` [("single 1", Right "OK")]
                `shouldBe` Right ()

        it "can disable single" $ do
            single False `with` [("single 0", Right "OK")]
                `shouldBe` Right ()

    describe "replayGainMode" $ do
        it "sets replay gain mode" $ do
            replayGainMode Off
                `with` [("replay_gain_mode off", Right "OK")]
                `shouldBe` Right ()

    describe "replayGainStatus" $ do
        it "returns the replay gain status" $ do
            replayGainStatus
                `with` [("replay_gain_status"
                        , Right "replay_gain_mode: off\nOK")]
                `shouldBe` Right ["replay_gain_mode: off"]

    describe "mixrampDb" $ do
        it "sends a mixrampdb request" $ do
            mixrampDb (-17)
                `with` [("mixrampdb -17", Right "OK")]
                `shouldBe` Right ()

    describe "mixrampDelay" $ do
         it "sends a mixrampdelay request" $ do
            mixrampDelay 20
                `with` [("mixrampdelay 20", Right "OK")]
                `shouldBe` Right ()
