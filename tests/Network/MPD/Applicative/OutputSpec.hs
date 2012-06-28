{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.OutputSpec (main, spec) where

import           TestUtil
import           Unparse

import           Network.MPD.Applicative.Output
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "disableOutput" $ do
        it "sends a disableoutput request" $ do
            disableOutput 1
                `with` [("disableoutput 1", Right "OK")]
                `shouldBe` Right ()

    describe "enableOutput" $ do
        it "sends an enableoutput request" $ do
            enableOutput 1
                `with` [("enableoutput 1", Right "OK")]
                `shouldBe` Right ()

    describe "outputs" $ do
        it "returns a list of audio output devices" $ do
            let obj = [Device 0 "SoundCard0" True
                      ,Device 1 "SoundCard1" False
                      ]
                resp = concatMap unparse obj ++ "OK"
            outputs
                `with` [("outputs", Right resp)]
                `shouldBe` Right obj
