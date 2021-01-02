{-# LANGUAGE OverloadedStrings #-}
module Network.MPD.Applicative.QuerySpec (main, spec) where

import           TestUtil
import           Unparse
import           Data.Time
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

import           Network.MPD.Applicative.Database
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "=?" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (Title =? "Bar")
                `with` [("find Title \"Bar\"", Right resp)]
                `shouldBe` Right [obj]
    describe "=? <> =?" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (Title =? "Bar" <> Artist =? "Baz")
                `with` [("find Title \"Bar\" Artist \"Baz\"", Right resp)]
                `shouldBe` Right [obj]
    describe "/=?" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (Title /=? "Bar")
                `with` [("find \"(Title != \\\"Bar\\\")\"", Right resp)]
                `shouldBe` Right [obj]
    describe "%?" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (Title %? "Bar")
                `with` [("find \"(Title contains \\\"Bar\\\")\"", Right resp)]
                `shouldBe` Right [obj]
    describe "~?" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (Title ~? "Bar")
                `with` [("find \"(Title =~ \\\"Bar\\\")\"", Right resp)]
                `shouldBe` Right [obj]
    describe "/~?" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (Title /~? "Bar")
                `with` [("find \"(Title !~ \\\"Bar\\\")\"", Right resp)]
                `shouldBe` Right [obj]
    describe "qFile" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (qFile "Bar.ogg")
                `with` [("find \"(file == \\\"Bar.ogg\\\")\"", Right resp)]
                `shouldBe` Right [obj]
    describe "qBase" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (qBase "Bar")
                `with` [("find \"(base \\\"Bar\\\")\"", Right resp)]
                `shouldBe` Right [obj]
    describe "qModSince" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (qModSince (UTCTime {utctDay = fromOrdinalDate 2005 348, utctDayTime = 40743}))
                `with` [("find \"(modified-since \\\"2005-12-14T11:19:03\\\")\"", Right resp)]
                `shouldBe` Right [obj]
    describe "qNot" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (qNot (Title =? "Bar"))
                `with` [("find \"(!(Title == \\\"Bar\\\"))\"", Right resp)]
                `shouldBe` Right [obj]
    describe "<>" $ do
        it "returns songs exactly matching a query" $ do
            let obj  = defaultSong "Foo.ogg"
                resp = unparse obj ++ "OK"
            find (Title =? "Bar" <> Artist /=? "Baz")
                `with` [("find \"((Title == \\\"Bar\\\") AND (Artist != \\\"Baz\\\"))\"", Right resp)]
                `shouldBe` Right [obj]
