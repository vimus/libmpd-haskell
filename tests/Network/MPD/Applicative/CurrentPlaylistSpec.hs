{-# LANGUAGE OverloadedStrings #-}
module Network.MPD.Applicative.CurrentPlaylistSpec (main, spec) where

import           TestUtil

import           Network.MPD.Applicative.CurrentPlaylist
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "addId" $ do
    it "adds a song to the playlist (non-recursive) and returns the song id" $ do
      addId "dir/Foo-Bar.ogg" Nothing
        `with` [("addid \"dir/Foo-Bar.ogg\"", Right "Id: 20\nOK")]
        `shouldBe` (Right $ Id 20)

    it "takes and optional position" $ do
      addId "dir/Foo-Bar.ogg" (Just 5)
        `with` [("addid \"dir/Foo-Bar.ogg\" 5", Right "Id: 20\nOK")]
        `shouldBe` (Right $ Id 20)

  describe "add" $ do
    it "adds a url to current playlist" $ do
      add "foo"
        `with` [("add \"foo\"", Right "OK")]
        `shouldBe` Right ()

  describe "clear" $ do
    it "clears current play list" $ do
      clear
        `with` [("clear", Right "OK")]
        `shouldBe` Right ()

  describe "delete" $ do
    it "deletes a song from the playlist" $ do
      delete (10 :: Int)
        `with` [("delete 10", Right "OK")]
        `shouldBe` Right ()

  describe "deleteRange" $ do
    it "deletes a range of songs from the playlist" $ do
      deleteRange (10, 20)
        `with` [("delete 10:20", Right "OK")]
        `shouldBe` Right ()

  describe "deleteId" $ do
    it "deletes song with given id from the playlist" $ do
      deleteId (Id 23)
        `with` [("deleteid 23", Right "OK")]
        `shouldBe` Right ()

  describe "move" $ do
    it "moves a song to a given position in the playlist" $ do
      move 23 42
        `with` [("move 23 42", Right "OK")]
        `shouldBe` Right ()

  describe "moveRange" $ do
    it "moves a range of songs to a given position in the playlist" $ do
      moveRange (10, 20) 23
        `with` [("move 10:20 23", Right "OK")]
        `shouldBe` Right ()

  describe "moveId" $ do
    it "move song with given id within the playlist" $ do
      moveId (Id 23) 10
        `with` [("moveid 23 10", Right "OK")]
        `shouldBe` Right ()
