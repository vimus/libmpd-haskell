{-# LANGUAGE OverloadedStrings #-}
module Network.MPD.Applicative.DatabaseSpec (main, spec) where

import           TestUtil
import           Unparse

import           Network.MPD.Applicative.Database
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "count" $ do
        it "returns a count of entries matching a query" $ do
            count (Title =? "Foo")
                `with` [("count Title \"Foo\""
                       , Right "songs: 0\nplaytime: 0\nOK")]
                `shouldBe` Right (Count 0 0)

    describe "find" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Bar.ogg"
                resp = unparse obj ++ "OK"
            find (Title =? "Foo")
                `with` [("find Title \"Foo\"", Right resp)]
                `shouldBe` Right [obj]

    describe "findadd" $ do
        it "adds songs matching query to current playlist" $ do
            findAdd (Title =? "Foo")
                `with` [("findadd Title \"Foo\"", Right "OK")]
                `shouldBe` Right ()

    describe "list" $ do
        it "lists all tags of the specified type" $ do
            list Title mempty
                `with` [("list Title"
                       , Right "Title: Foo\nTitle: Bar\nOK"
                       )]
                `shouldBe` Right ["Foo", "Bar"]

        it "can list albums by an artist" $ do
            list Album (Artist =? "Muzz")
                `with` [("list Album Artist \"Muzz\""
                       , Right "Album: Foo\nOK")]
                `shouldBe` Right ["Foo"]

        it "can filter tags by arbitrary queries" $ do
            list Title (qNot $ Artist =? "Foo" <> Album =? "Bar")
                 `with` [("list Title \"(!((Artist == \\\"Foo\\\") AND (Album == \\\"Bar\\\")))\""
                         , Right "Title: Foo\nOK")]
                 `shouldBe` Right ["Foo"]

    describe "listAll" $ do
        it "recursively lists songs in a database directory" $ do
            listAll ""
                `with` [("listall \"\""
                       , Right "directory: FooBand\n\
                               \directory: FooBand/album1\n\
                               \file: FooBand/album1/01 - songA.ogg\n\
                               \file: FooBand/album1/02 - songB.ogg\nOK")]
                `shouldBe` Right ["FooBand/album1/01 - songA.ogg"
                                 ,"FooBand/album1/02 - songB.ogg"]

    describe "lsInfo" $ do
        it "returns a non-recursive listing of a database directory" $ do
            let song = defaultSong "Bar.ogg"
                resp = "directory: Foo\n" ++ unparse song
                       ++ "\nplaylist: Quux\nOK"
            lsInfo ""
                `with` [("lsinfo \"\""
                       , Right resp)]
                `shouldBe` Right [ LsDirectory "Foo"
                                 , LsSong song
                                 , LsPlaylist "Quux"]

    describe "listAllInfo" $ do
        it "is a recursive 'lsInfo'" $ do
            listAllInfo ""
                `with` [("listallinfo \"\""
                       , Right "directory: Foo\ndirectory: Bar\nOK"
                       )]
                `shouldBe` Right [LsDirectory "Foo", LsDirectory "Bar"]

    describe "search" $ do
        it "returns songs matching a case-insensitive query" $ do
            let obj  = defaultSong "Bar.ogg"
                resp = unparse obj ++ "OK"
            search (Title =? "Foo")
                `with` [("search Title \"Foo\"", Right resp)]
                `shouldBe` Right [obj]

    describe "searchAdd" $ do
        it "sends a searchadd request" $ do
            searchAdd (Title =? "Foo")
                `with` [("searchadd Title \"Foo\"", Right "OK")]
                `shouldBe` Right ()

    describe "searchAddPl" $ do
        it "sends a searchaddpl request" $ do
            searchAddPl "Foo" (Title =? "Bar")
                `with` [("searchaddpl \"Foo\" Title \"Bar\"", Right "OK")]
                `shouldBe` Right ()

    describe "update" $ do
        it "updates the entire collection by default" $ do
            update Nothing
                `with` [("update", Right "updating_db: 23\nOK")]
                `shouldBe` Right 23

        it "can update a specific path" $ do
            update (Just "foo")
                `with` [("update \"foo\"", Right "updating_db: 23\nOK")]
                `shouldBe` Right 23

    describe "rescan" $ do
        it "returns entire collection by default" $ do
            rescan Nothing
                `with` [("rescan", Right "updating_db: 23\nOK")]
                `shouldBe` Right 23

        it "can rescan a specific path" $ do
            rescan (Just "foo")
                `with` [("rescan \"foo\"", Right "updating_db: 23\nOK")]
                `shouldBe` Right 23
