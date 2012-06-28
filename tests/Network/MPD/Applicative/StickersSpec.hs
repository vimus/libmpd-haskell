{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.StickersSpec (main, spec) where

import           TestUtil

import           Network.MPD.Applicative.Stickers
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "stickerGet" $ do
        it "reads a sticker value" $ do
            stickerGet SongObj "foo/bar/baz.ogg" "test"
                `with` [("sticker get song \"foo/bar/baz.ogg\"\
                         \ \"test\"", Right "OK")]
                `shouldBe` Right []

    describe "stickerSet" $ do
        it "sets a sticker value" $ do
            stickerSet SongObj "foo/bar/baz.ogg" "test" "tast"
                `with` [("sticker set song \"foo/bar/baz.ogg\"\
                         \ \"test\" \"tast\"", Right "OK")]
                `shouldBe` Right ()

    describe "stickerDelete" $ do
        it "deletes a sticker" $ do
            stickerDelete SongObj "foo/bar/baz.ogg" "test"
                `with` [("sticker delete song \"foo/bar/baz.ogg\"\
                         \ \"test\"", Right "OK")]
                `shouldBe` Right ()

    describe "stickerList" $ do
        it "returns a list of stickers" $ do
            stickerList SongObj "foo/bar/baz.ogg"
                `with` [("sticker list song \"foo/bar/baz.ogg\""
                       , Right "OK")]
                `shouldBe` Right []

    describe "stickerFind" $ do
        it "returns a list of matching stickers" $ do
            stickerFind SongObj "foo/bar/baz.ogg" "test"
                `with` [("sticker find song \"foo/bar/baz.ogg\"\
                         \ \"test\""
                       , Right "OK")]
                `shouldBe` Right []
