{-# LANGUAGE OverloadedStrings #-}

module Network.MPD.Applicative.CurrentPlaylistSpec (main, spec) where

import           TestUtil
import           Unparse

import           Network.MPD.Applicative.CurrentPlaylist
import           Network.MPD.Commands.Query
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

  -- XXX: generalize to arbitrary SongS and Query
  describe "playlistFind" $ do
    it "searches for songs in the current playlist" $ do
      let obj = defaultSong "Foo.ogg"
          resp = unparse obj
      playlistFind (Artist =? "Foo")
        `with` [("playlistfind Artist \"Foo\"", Right $ resp ++ "\nOK")]
        `shouldBe` Right [obj]

  -- XXX: generalize to arbitrary SongS
  describe "playlistInfo" $ do
    it "retrieves metadata for all songs in the current playlist" $ do
      let obj = defaultSong "Foo.ogg"
          resp = unparse obj
      playlistInfo Nothing
        `with` [("playlistinfo", Right $ resp ++ "\nOK")]
        `shouldBe` Right [obj]

    it "can optionally return only songs within a range" $ do
      let obj = [ defaultSong "Foo.ogg"
                , defaultSong "Bar.ogg"
                ]
          resp = unlines $ map unparse obj
      playlistInfo (Just (0, 1))
        `with` [("playlistinfo 0:1", Right $ resp ++ "\nOK")]
        `shouldBe` Right [ obj !! 0
                         , obj !! 1
                         ]

  -- XXX: generlize to arbitrary SongS
  describe "playlistId" $ do
    it "retrieves metadata for all songs in the current playlist" $ do
      let obj = defaultSong "Foo.ogg"
          resp = unparse obj
      playlistId Nothing
        `with` [("playlistid", Right $ resp ++ "\nOK")]
        `shouldBe` Right [obj]

    it "can optionally return info only for a position" $ do
      let obj = defaultSong "Foo.ogg"
          resp = unparse obj
      playlistId (Just $ Id 0)
        `with` [("playlistid 0", Right $ resp ++ "\nOK")]
        `shouldBe` Right [obj]

  describe "playlistSearch" $ do
    it "returns songs matching an inexact query" $ do
      let obj = defaultSong "Foo.ogg"
          resp = unparse obj
      playlistSearch (Title =? "Foo")
        `with` [("playlistsearch Title \"Foo\"", Right $ resp ++ "\nOK")]
        `shouldBe` Right [obj]

  describe "plChanges" $ do
    it "returns songs that have changed since the given playlist version" $ do
      let obj = defaultSong "foo.ogg"
      plChanges 1
        `with` [("plchanges 1"
               , Right (unparse obj ++ "OK"))
               ]
        `shouldBe` Right [obj]

  describe "plChangesPosId" $ do
    it "is like plChanges but only returns positions and ids" $ do
      plChangesPosId 1
        `with` [("plchangesposid 1"
               , Right "cpos: 0\n\
                       \Id: 0\n\
                       \OK")]
        `shouldBe` Right [(0, Id 0)]

{- XXX: doesn't work
    it "fails on weird input" $ do
      plChangesPosId 10
        `with` [("plchangesposid 10", Right "cpos: foo\nId: bar\nOK")]
        `shouldBe` Left (Unexpected "[(\"cpos\",\"foo\"),(\"Id\",\"bar\")]")
-}

  describe "shuffle" $ do
    it "shuffles the current playlist" $ do
      shuffle Nothing
        `with` [("shuffle", Right "OK")]
        `shouldBe` Right ()

    it "optionally shuffles a selection of the playlist" $ do
      shuffle (Just (15, 25))
        `with` [("shuffle 15:25", Right "OK")]
        `shouldBe` Right ()

  describe "swap" $ do
    it "swaps two playlist positions" $ do
      swap 1 2
      `with` [("swap 1 2", Right "OK")]
      `shouldBe` Right ()

  describe "swapId" $ do
    it "swaps two playlist ids" $ do
      swapId (Id 1) (Id 2)
        `with` [("swapid 1 2", Right "OK")]
        `shouldBe` Right ()
