{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.ReflectionSpec (main, spec) where

import           TestUtil

import           Network.MPD.Applicative.Reflection

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "commands" $ do
        it "returns a list of commands" $ do
            commands
                `with` [("commands"
                       , Right "command: foo\ncommand: bar\nOK")]
                `shouldBe` Right ["foo", "bar"]

    describe "notCommands" $ do
        it "returns a list of commands" $ do
            notCommands
                `with` [("notcommands"
                       , Right "command: foo\ncommand: bar\nOK")]
                `shouldBe` Right ["foo", "bar"]

    describe "tagTypes" $ do
        it "returns a list of tag types" $ do
            tagTypes
                `with` [("tagtypes"
                       , Right "tagtype: foo\ntagtype: bar\nOK")]
                `shouldBe` Right ["foo", "bar"]

    describe "urlHandlers" $ do
        it "returns a list of url handlers" $ do
            urlHandlers
                `with` [("urlhandlers"
                       , Right "urlhandler: foo\nurlhandler: bar\nOK")]
                `shouldBe` Right ["foo", "bar"]

    describe "decoders" $ do
        it "returns a list of decoders and suffixes" $ do
            decoders
                `with` [("decoders"
                       , Right "plugin: mad\n\
                               \suffix: mp3\n\
                               \suffix: mp2\n\
                               \mime_type: audio/mpeg\n\
                               \plugin: vorbis\n\
                               \suffix: ogg\n\
                               \suffix: oga\n\
                               \mime_type: application/ogg\n\
                               \mime_type: application/x-ogg\n\
                               \mime_type: audio/ogg\n\
                               \OK")]
                `shouldBe` Right [("mad", [("suffix", "mp3")
                                          ,("suffix", "mp2")
                                          ,("mime_type", "audio/mpeg")])
                                 ,("vorbis", [("suffix", "ogg")
                                             ,("suffix", "oga")
                                             ,("mime_type", "application/ogg")
                                             ,("mime_type", "application/x-ogg")
                                             ,("mime_type", "audio/ogg")])
                                 ]
