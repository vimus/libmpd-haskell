{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- This module provides a way of verifying that the interface to the MPD
-- commands is correct. It does so by capturing the data flow between the
-- command and a dummy socket, checking the captured data against a set of
-- predefined values that are known to be correct. Of course, this does not
-- verify that the external behaviour is correct, it's simply a way of
-- catching silly mistakes and subtle bugs in the interface itself, without
-- having to actually send any requests to a real server.

module CommandSpec (main, spec) where

import           Arbitrary ()
import           StringConn
import           TestUtil
import           Unparse

import           Network.MPD.Core
import           Network.MPD.Commands
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Extensions

import           Prelude hiding (repeat)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    -- * Admin commands
    describe "enableOutput" $ do
        it "sends an enableoutput command" $ testEnableOutput

    describe "disableOutput" $ do
        it "sends an disableoutput command" $ testDisableOutput

    describe "outputs" $ do
        it "lists available outputs" $ testOutputs

    describe "update" $ do
        it "updates entire collection by default" $ do
            cmd [("update", Right "updating_db: 23\nOK")] (Right 23)
                (update Nothing)

        it "can update a specific path" $ do
            cmd [("update \"foo\"", Right "updating_db: 23\nOK")] (Right 23)
                (update $ Just "foo")

    describe "rescan" $ do
        it "returns entire collection by default" $ do
            cmd [("rescan", Right "updating_db: 23\nOK")] (Right 23)
                (rescan Nothing)

        it "can rescan a specific path" $ do
            cmd [("rescan \"foo\"", Right "updating_db: 23\nOK")] (Right 23)
                (rescan $ Just "foo")

    -- * Database commands

{- XXX: incorrect; fixed in the applicative version
    describe "list" $ do
        it "returns a list of values for a given metadata type" $ testListAny
        it "can constrain listing to entries matching a query" $ testListQuery
-}

    describe "listAll" $ do
        it "lists everything" $ testListAll

    describe "lsInfo" $ do
       it "lists information" $ testLsInfo

    describe "listAllInfo" $ do
        it "lists information" $ testListAllInfo

    describe "count" $ do
        it "returns a count of items matching a query" $ testCount

    -- * Playlist commands
    describe "playlistAdd" $ do
        it "adds a url to a stored playlist" $ testPlaylistAdd

    describe "playlistClear" $ do
        it "clears a stored playlist" $ testPlaylistClear

    describe "plChangesPosid" $ do
        it "does something ..." $ testPlChangesPosId
{- XXX: doesn't work
        it "fails on weird input" $ testPlChangesPosIdWeird
-}

    -- XXX: this is ill-defined
    {-
    describe "currentSong" $ do
        it "can handle cases where playback is stopped" $ testCurrentSong
     -}

    describe "playlistDelete" $ do
        it "deletes an item from a stored playlist" $ testPlaylistDelete

    describe "load" $ do
        it "loads a stored playlist" $ testLoad

    describe "playlistMove" $ do
        it "moves an item within a stored playlist" $ testMove2
    describe "rm" $ do
        it "deletes a stored playlist" $ testRm
    describe "rename" $ do
        it "renames a stored playlist" $ testRename
    describe "save" $ do
        it "creates a stored playlist" $ testSave
    describe "shuffle" $ do
        it "enables shuffle mode" $ testShuffle
    describe "listPlaylist" $ do
        it "returns a listing of paths in a stored playlist" $ testListPlaylist

    -- * Playback commands

    describe "crossfade" $ do
        it "sets crossfade between songs" $ testCrossfade
    describe "play" $ do
        it "resumes playback" $ testPlay
    describe "pause" $ do
        it "pauses playback" $ testPause
    describe "toggle" $ do
        it "toggles playback" $ testToggle
    describe "stop" $ do
        it "stops playback" $ testStop
    describe "next" $ do
        it "starts playback of next song" $ testNext
    describe "previous" $ do
        it "play previous song" $ testPrevious
    describe "random" $ do
        it "toggles random playback" $ testRandom
    describe "repeat" $ do
        it "toggles repeating playback" $ testRepeat
    describe "setVolume" $ do
        it "sets playback volume" $ testSetVolume
    describe "consume" $ do
        it "toggles consume mode" $ testConsume
    describe "single" $ do
        it "toggles single mode" $ testSingle

    -- * Misc
    describe "clearError" $ do
        it "removes errors" $ testClearError
    describe "commands" $ do
        it "lists available commands" $ testCommands
    describe "notCommands" $ do
        it "lists unavailable commands" $ testNotCommands
    describe "tagTypes" $ do
        it "lists available tag types" $ testTagTypes
    describe "urlHandlers" $ do
        it "lists available url handlers" $ testUrlHandlers
    describe "password" $ do
        it "sends a password to the server" $ testPassword
        it "gives access to restricted commmands" $ testPasswordSucceeds
        it "returns failure on incorrect password" $ testPasswordFails
    describe "ping" $ do
        it "sends a ping command" $ testPing
    describe "stats" $ do
        it "gets database stats" $ testStats

    -- * Extensions
    describe "addMany" $ do
        it "adds multiple paths in one go" $ testAddMany0
        it "can also add to stored playlists" $ testAddMany1
    describe "volume" $ do
        it "adjusts volume relative to current volume" $ testVolume


cmd_ :: [(Expect, Response String)] -> StringMPD () -> Expectation
cmd_ expect f     = cmd expect (Right ()) f

cmd :: (Eq a, Show a) => [(Expect, Response String)] -> Response a -> StringMPD a -> Expectation
cmd expect resp f = testMPD expect f `shouldBe` resp

--
-- Admin commands
--

testEnableOutput  = cmd_ [("enableoutput 1", Right "OK")] (enableOutput 1)
testDisableOutput = cmd_ [("disableoutput 1", Right "OK")] (disableOutput 1)

-- XXX: this should be generalized to arbitrary inputs
testOutputs = do
    let obj1 = def { dOutputName = "SoundCard0", dOutputEnabled = True }
        obj2 = def { dOutputName = "SoundCard1", dOutputID = 1 }
        resp = concatMap unparse [obj1, obj2] ++ "OK"
    cmd [("outputs", Right resp)] (Right [obj1, obj2]) outputs



--
-- Database commands
--

{- XXX: this is incorrect
-- XXX: generalize to arbitrary Metadata values
testListAny = cmd [("list Title", Right "Title: Foo\nTitle: Bar\nOK")]
                  (Right ["Foo", "Bar"])
                  (list Title anything)

testListQuery = cmd [("list Title Artist \"Muzz\"", Right "Title: Foo\nOK")]
                    (Right ["Foo"])
                    (list Title (Artist =? "Muzz"))
-}

testListAll =
    cmd [("listall \"\"", Right "directory: FooBand\n\
                                \directory: FooBand/album1\n\
                                \file: FooBand/album1/01 - songA.ogg\n\
                                \file: FooBand/album1/02 - songB.ogg\nOK")]
        (Right ["FooBand/album1/01 - songA.ogg"
               ,"FooBand/album1/02 - songB.ogg"])
        (listAll "")

-- XXX: generalize to arbitrary input
testLsInfo = do
    let song = defaultSong "Bar.ogg"
    cmd [("lsinfo \"\"", Right $ "directory: Foo\n" ++ unparse song ++ "playlist: Quux\nOK")]
        (Right [LsDirectory "Foo", LsSong song, LsPlaylist "Quux"])
        (lsInfo "")

testListAllInfo =
    cmd [("listallinfo \"\"", Right "directory: Foo\ndirectory: Bar\nOK")]
        (Right [LsDirectory "Foo", LsDirectory "Bar"])
        (listAllInfo "")

-- XXX: generalize to arbitrary input
testCount = do
    let obj  = Count 1 60
        resp = unparse obj ++ "OK"
    cmd [("count Title \"Foo\"", Right resp)] (Right obj)
        (count (Title =? "Foo"))

--
-- Playlist commands
--
testPlaylistAdd =
    cmd_ [("playlistadd \"foo\" \"bar\"", Right "OK")]
         (playlistAdd "foo" "bar")

testPlaylistClear =
    cmd_ [("playlistclear \"foo\"", Right "OK")]
         (playlistClear "foo")

testPlChangesPosId =
    cmd [("plchangesposid 10", Right "OK")]
        (Right [])
        (plChangesPosId 10)

{- XXX:
testPlChangesPosIdWeird =
    cmd [("plchangesposid 10", Right "cpos: foo\nId: bar\nOK")]
        (Left $ Unexpected "[(\"cpos\",\"foo\"),(\"Id\",\"bar\")]")
        (plChangesPosId 10)
-}

-- XXX: this is ill-defined
{-
testCurrentSong = do
    let obj  = def { stState = Stopped, stPlaylistVersion = 253 }
        resp = unparse obj ++ "OK"
    cmd [("status", Right resp)] (Right Nothing) currentSong
-}

testPlaylistDelete =
    cmd_ [("playlistdelete \"foo\" 1", Right "OK")] (playlistDelete "foo" 1)

testLoad =
    cmd_ [("load \"foo\"", Right "OK")] (load "foo")

testMove2 = cmd_ [("playlistmove \"foo\" 23 2", Right "OK")] (playlistMove "foo" (Id 23) 2)

testRm = cmd_ [("rm \"foo\"", Right "OK")] (rm "foo")

testRename = cmd_ [("rename \"foo\" \"bar\"", Right "OK")] (rename "foo" "bar")

testSave = cmd_ [("save \"foo\"", Right "OK")] (save "foo")

testShuffle = cmd_ [("shuffle", Right "OK")] (shuffle Nothing)

testListPlaylist = cmd [("listplaylist \"foo\""
                         ,Right "file: dir/Foo-bar.ogg\n\
                                \file: dir/Quux-quuz.ogg\n\
                                \OK")]
                   (Right ["dir/Foo-bar.ogg", "dir/Quux-quuz.ogg"])
                   (listPlaylist "foo")

--
-- Playback commands
--

testCrossfade = cmd_ [("crossfade 0", Right "OK")] (crossfade 0)

testPlay = cmd_ [("play", Right "OK")] (play Nothing)

testPause = cmd_ [("pause 0", Right "OK")] (pause False)

testToggle = cmd_ [("pause", Right "OK")] (toggle)

testStop = cmd_ [("stop", Right "OK")] stop

testNext = cmd_ [("next", Right "OK")] next

testPrevious = cmd_ [("previous", Right "OK")] previous

testRandom = cmd_ [("random 0", Right "OK")] (random False)

testRepeat = cmd_ [("repeat 0", Right "OK")] (repeat False)

testSetVolume = cmd_ [("setvol 10", Right "OK")] (setVolume 10)

testConsume = cmd_ [("consume 1", Right "OK")] (consume True)

testSingle = cmd_ [("single 1", Right "OK")] (single True)

--
-- Miscellaneous commands
--

testClearError = cmd_ [("clearerror", Right "OK")] clearError

testCommands =
    cmd [("commands", Right "command: foo\ncommand: bar")]
        (Right ["foo", "bar"])
        commands

testNotCommands =
    cmd [("notcommands", Right "command: foo\ncommand: bar")]
         (Right ["foo", "bar"])
         notCommands

testTagTypes =
    cmd [("tagtypes", Right "tagtype: foo\ntagtype: bar")]
         (Right ["foo", "bar"])
         tagTypes

testUrlHandlers =
    cmd [("urlhandlers", Right "urlhandler: foo\nurlhandler: bar")]
         (Right ["foo", "bar"])
         urlHandlers

testPassword = cmd_ [("password foo", Right "OK")] (password "foo")

testPasswordSucceeds =
    testMPDWithPassword convo "foo" cmd_in `shouldBe` expected_resp
    where
        convo = [("lsinfo \"/\"", Right "ACK [4@0] {play} you don't have \
                                        \permission for \"play\"")
                ,("password foo", Right "OK")
                ,("lsinfo \"/\"", Right "directory: /bar\nOK")]
        expected_resp = Right [LsDirectory "/bar"]
        cmd_in = lsInfo "/"

testPasswordFails =
    testMPDWithPassword convo "foo" cmd_in `shouldBe` expected_resp
    where
        convo = [("play", Right "ACK [4@0] {play} you don't have \
                                \permission for \"play\"")
                ,("password foo",
                  Right "ACK [3@0] {password} incorrect password")]
        expected_resp =
            Left $ ACK InvalidPassword " incorrect password"
        cmd_in = play Nothing

testPing = cmd_ [("ping", Right "OK")] ping

testStats = cmd [("stats", Right resp)] (Right obj) stats
    where obj = def { stsArtists = 1, stsAlbums = 1, stsSongs =  1
                    , stsUptime = 100, stsPlaytime = 100, stsDbUpdate = 10
                    , stsDbPlaytime = 100 }
          resp = unparse obj ++ "OK"

--
-- Extensions\/shortcuts
--

testAddMany0 = cmd_ [("add \"bar\"", Right "OK")]
               (addMany "" ["bar"])

testAddMany1 = cmd_ [("playlistadd \"foo\" \"bar\"", Right "OK")]
               (addMany "foo" ["bar"])

testVolume = cmd_ [("status", Right st), ("setvol 90", Right "OK")] (volume (-10))
    where st = unparse def { stVolume = Just 100 }
