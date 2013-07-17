{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.StoredPlaylistsSpec (main, spec) where

import           TestUtil
import           Unparse

import           Network.MPD.Applicative.StoredPlaylists
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "listPlaylist" $ do
        it "returns a list of uris in a playlist" $ do
            listPlaylist "foo"
                `with` [("listplaylist \"foo\""
                       , Right "file: foo.ogg\n\
                               \file: bar.ogg\n\
                               \OK")]
                `shouldBe` Right ["foo.ogg", "bar.ogg"]

    describe "listPlaylistInfo" $ do
        it "returns metadata for songs in a playlist" $ do
            let obj = defaultSong "foo.ogg"
            listPlaylistInfo "foo"
                `with` [("listplaylistinfo \"foo\""
                       , Right (unparse obj ++ "OK")
                       )]
                `shouldBe` Right [obj]

    describe "listPlaylists" $ do
        it "returns a list of playlists" $ do
            listPlaylists
                `with` [("listplaylists"
                       , Right "playlist: foo\n\
                               \Last-Modified: 2011-12-03T08:53:09Z\n\
                               \playlist: bar\n\
                               \Last-Modified: 2012-12-03T09:57:09Z\n\
                               \OK"
                       )]
                `shouldBe` Right ["bar", "foo"]

    describe "load" $ do
        it "sends a load request" $ do
            load "foo" `with` [("load \"foo\"", Right "OK")]
                `shouldBe` Right ()

    describe "playlistAdd" $ do
        it "sends a playlistadd request" $ do
            playlistAdd "foo" "foo.ogg"
                `with` [("playlistadd \"foo\" \"foo.ogg\"", Right "OK")]
                `shouldBe` Right ()

    describe "playlistClear" $ do
        it "sends a playlistclear request" $ do
            playlistClear "foo"
                `with` [("playlistclear \"foo\"", Right "OK")]
                `shouldBe` Right ()

    describe "playlistDelete" $ do
        it "sends a playlistdelete request" $ do
            playlistDelete "foo" 10
                `with` [("playlistdelete \"foo\" 10", Right "OK")]
                `shouldBe` Right ()

    describe "playlistMove" $ do
        it "sends a playlistmove request" $ do
            playlistMove "foo" (Id 10) 20
                `with` [("playlistmove \"foo\" 10 20", Right "OK")]
                `shouldBe` Right ()

    describe "rename" $ do
        it "sends a rename request" $ do
            rename "foo" "bar"
                `with` [("rename \"foo\" \"bar\"", Right "OK")]
                `shouldBe` Right ()

    describe "save" $ do
        it "sends a save request" $ do
            save "foo"
                `with` [("save \"foo\"", Right "OK")]
                `shouldBe` Right ()
