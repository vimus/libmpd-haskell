{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.ConnectionSpec (main, spec) where

import           TestUtil

import           Network.MPD.Applicative.Connection
import           Network.MPD.Applicative.Database
import           Network.MPD.Applicative.PlaybackControl
import           Network.MPD.Core
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "password" $ do
        it "sends a password" $ do
            password "foo"
                `with` [("password foo", Right "OK")]
                `shouldBe` Right ()

        it "authenticates a session" $ do
            let convo = [("lsinfo \"/\""
                         , Right "ACK [4@0] {play} you don't have \
                                 \permission for \"play\"")
                        ,("password foo", Right "OK")
                        ,("lsinfo \"/\"", Right "directory: /bar\nOK")]
                expected_resp = Right [LsDirectory "/bar"]
                cmd_in = lsInfo "/"
            withPassword "foo" convo cmd_in `shouldBe` expected_resp

        it "fails if the password is incorrect" $ do
            let convo = [("play"
                         , Right "ACK [4@0] {play} you don't have \
                                 \permission for \"play\"")
                        ,("password foo"
                         , Right "ACK [3@0] {password} incorrect password")]
                expected_resp =
                    Left $ ACK InvalidPassword " incorrect password"
                cmd_in = play Nothing
            withPassword "foo" convo cmd_in `shouldBe` expected_resp

    describe "ping" $ do
        it "sends a ping" $ do
            ping `with` [("ping", Right "OK")] `shouldBe` Right ()
