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
