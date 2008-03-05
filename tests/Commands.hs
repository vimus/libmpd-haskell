{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- |
-- This module provides a way of verifying that the interface to the MPD
-- commands is correct. It does so by capturing the data flow between the
-- command and a dummy socket, checking the captured data against a set of
-- predefined values that are known to be correct. Of course, this does not
-- verify that the external behaviour is correct, it's simply a way of
-- catching silly mistakes and subtle bugs in the interface itself, without
-- having to actually send any requests to a real server.

module Main (main) where

import Network.MPD.Commands
import Network.MPD.Core (Response)
import Network.MPD.StringConn

import Control.Monad
import Prelude hiding (repeat)
import Text.Printf

main = mapM_ (\(n, f) -> f >>= \x -> printf "%-14s: %s\n" n x) tests
    where tests = [("enableOutput", testEnableOutput)
                  ,("disableOutput", testDisableOutput)
                  ,("outputs", testOutputs)
                  ,("update0", testUpdate0)
                  ,("update1", testUpdate1)
                  ,("updateMany", testUpdateMany)
                  ,("find", testFind)
                  ,("list(Nothing)", testListNothing)
                  ,("list(Just)", testListJust)
                  ,("listAll", testListAll)
                  ,("lsInfo", testLsInfo)
                  ,("listAllInfo", testListAllInfo)
                  ,("search", testSearch)
                  ,("count", testCount)
                  ,("add", testAdd)
                  ,("add_", testAdd_)
                  ,("addId", testAddId)
                  ,("clear", testClear)
                  ,("currentSong(_)", testCurrentSongStopped)
                  ,("currentSong(>)", testCurrentSongPlaying)
                  ,("delete0", testDelete0)
                  ,("delete1", testDelete1)
                  ,("delete2", testDelete2)
                  ,("load", testLoad)
                  ,("move0", testMove0)
                  ,("move1", testMove1)
                  ,("move2", testMove2)
                  ,("rm", testRm)
                  ,("rename", testRename)
                  ,("save", testSave)
                  ,("swap0", testSwap0)
                  ,("swap1", testSwap1)
                  ,("shuffle", testShuffle)
                  ,("crossfade", testCrossfade)
                  ,("play", testPlay)
                  ,("pause", testPause)
                  ,("stop", testStop)
                  ,("next", testNext)
                  ,("previous", testPrevious)
                  ,("random", testRandom)
                  ,("repeat", testRepeat)
                  ,("setVolume", testSetVolume)
                  ,("volume", testVolume)
                  ,("clearError", testClearError)
                  ,("commands", testCommands)
                  ,("notCommands", testNotCommands)
                  ,("tagTypes", testTagTypes)
                  ,("urlHandlers", testUrlHandlers)
                  ,("ping", testPing)
                  ,("song parsing / incomplete track",
                    testSongParseIncompleteTrack)
                  ,("song parsing / complete track",
                    testSongParseCompleteTrack)
                  ]

test a b c = liftM (showResult b) $ testMPD a b (return Nothing) c

test_ a b = test a (Right ()) b

showResult :: (Show a) => Response a -> Result a -> String
showResult _ Ok = "passed"
showResult expectedResult (Failure result mms) =
    "*** FAILURE ***" ++
    concatMap (\(x,y) -> "\n  expected request: " ++ show x ++
                         "\n  actual request: " ++ show y) mms ++
    "\n    expected result: " ++ show expectedResult ++
    "\n    actual result: " ++ show result

emptySong = Song { sgArtist   = ""
                 , sgAlbum    = ""
                 , sgTitle    = ""
                 , sgFilePath = ""
                 , sgGenre    = ""
                 , sgName     = ""
                 , sgComposer = ""
                 , sgPerformer = ""
                 , sgLength   = 0
                 , sgDate     = 0
                 , sgTrack    = (0,0)
                 , sgDisc     = (0,0)
                 , sgIndex    = Nothing }

--
-- Parser behaviour.
-- These tests are meant to expose problems with internal
-- parsers.
--

-- Should handle track = 'X'.
testSongParseIncompleteTrack =
    test [("find Artist \"Foo\"", Right "file: dir/Foo-Bar.ogg\n\
                                        \Track: 1\n\
                                        \OK")]
         (Right [emptySong { sgTrack = (1,1)
                           , sgFilePath = "dir/Foo-Bar.ogg"
                           }])
         (find $ Query Artist "Foo")

-- Should handle track = 'X/Y'.
testSongParseCompleteTrack =
    test [("find Artist \"Foo\"", Right "file: dir/Foo-Bar.ogg\n\
                                        \Track: 2/12\n\
                                        \OK")]
         (Right [emptySong { sgTrack = (2,12)
                           , sgFilePath = "dir/Foo-Bar.ogg"
                           }])
         (find $ Query Artist "Foo")

--
-- Admin commands
--

testEnableOutput = test_ [("enableoutput 1", Right "OK")] (enableOutput 1)

testDisableOutput = test_ [("disableoutput 1", Right "OK")] (disableOutput 1)

testOutputs =
    test [("outputs", Right $ unlines ["outputid: 0"
                                      ,"outputname: SoundCard0"
                                      ,"outputenabled: 1"
                                      ,"outputid: 1"
                                      ,"outputname: SoundCard1"
                                      ,"outputenabled: 0"
                                      ,"OK"])]
         (Right [Device { dOutputID = 0
                        , dOutputName = "SoundCard0"
                        , dOutputEnabled = True }
                ,Device { dOutputID = 1
                        , dOutputName = "SoundCard1"
                        , dOutputEnabled = False }])
         outputs

testUpdate0 = test_ [("update", Right "updating_db: 1\nOK")] (update [])

testUpdate1 =
    test_ [("update \"foo\"", Right "updating_db: 1\nOK")]
          (update ["foo"])

testUpdateMany =
    test_ [("command_list_begin\nupdate \"foo\"\nupdate \"bar\"\n\
            \command_list_end", Right "updating_db: 1\nOK")]
          (update ["foo","bar"])

--
-- Database commands
--

testFind =
    test [("find Artist \"Foo\"", Right "file: dir/Foo-Bar.ogg\n\
                                        \Time: 60\n\
                                        \Artist: Foo\n\
                                        \Title: Bar\n\
                                        \OK")]
         (Right [Song { sgArtist    = "Foo"
                      , sgAlbum     = ""
                      , sgTitle     = "Bar"
                      , sgFilePath  = "dir/Foo-Bar.ogg"
                      , sgGenre     = ""
                      , sgName      = ""
                      , sgComposer  = ""
                      , sgPerformer = ""
                      , sgLength    = 60
                      , sgDate      = 0
                      , sgTrack     = (0,0)
                      , sgDisc      = (0,0)
                      , sgIndex     = Nothing
                      }])
         (find (Query Artist "Foo"))

testListNothing =
    test [("list Title", Right "Title: Foo\nTitle: Bar\nOK")]
         (Right ["Foo", "Bar"])
         (list Title Nothing)

testListJust =
    test [("list Title Artist \"Muzz\"", Right "Title: Foo\nOK")]
         (Right ["Foo"])
         (list Title (Just $ Query Artist "Muzz"))

testListAll =
    test [("listall \"\"", Right "directory: FooBand\n\
                                 \directory: FooBand/album1\n\
                                 \file: FooBand/album1/01 - songA.ogg\n\
                                 \file: FooBand/album1/02 - songB.ogg\nOK")]
         (Right ["FooBand/album1/01 - songA.ogg"
                ,"FooBand/album1/02 - songB.ogg"])
         (listAll "")

testLsInfo =
    test [("lsinfo \"\"", Right "directory: Foo\ndirectory: Bar\nOK")]
         (Right [Left "Bar", Left "Foo"])
         (lsInfo "")

testListAllInfo =
    test [("listallinfo \"\"", Right "directory: Foo\ndirectory: Bar\nOK")]
         (Right [Left "Bar", Left "Foo"])
         (listAllInfo "")

testSearch =
    test [("search Artist \"oo\"", Right "file: dir/Foo-Bar.ogg\n\
                                         \Time: 60\n\
                                         \Artist: Foo\n\
                                         \Title: Bar\n\
                                         \OK")]
         (Right [Song { sgArtist    = "Foo"
                      , sgAlbum     = ""
                      , sgTitle     = "Bar"
                      , sgFilePath  = "dir/Foo-Bar.ogg"
                      , sgGenre     = ""
                      , sgName      = ""
                      , sgComposer  = ""
                      , sgPerformer = ""
                      , sgLength    = 60
                      , sgDate      = 0
                      , sgTrack     = (0,0)
                      , sgDisc      = (0,0)
                      , sgIndex     = Nothing
                      }])
         (search (Query Artist "oo"))

testCount =
    test [("count Title \"Foo\"", Right "songs: 1\nplaytime: 60\nOK")]
         (Right (Count 1 60))
         (count (Query Title "Foo"))

--
-- Playlist commands
--

testAdd =
    test [("add \"foo\"", Right "OK"),
          ("listall \"foo\"", Right "file: Foo\nfile: Bar\nOK")]
         (Right ["Foo", "Bar"])
         (add "" "foo")

testAdd_ = test_ [("add \"foo\"", Right "OK")] (add_ "" "foo")

testAddId =
    test [("addid \"dir/Foo-Bar.ogg\"", Right "Id: 20\nOK")]
         (Right 20)
         (addId "dir/Foo-Bar.ogg")

testClear = test_ [("playlistclear \"foo\"", Right "OK")] (clear "foo")

testCurrentSongStopped =
    test [("status", Right "repeat: 0\n\
                           \random: 0\n\
                           \playlist: 253\n\
                           \playlistlength: 0\n\
                           \xfade: 0\n\
                           \state: stop\nOK")]
         (Right Nothing)
         (currentSong)

testCurrentSongPlaying =
    test [("status", Right "volume: 80\n\
                           \repeat: 0\n\
                           \random: 0\n\
                           \playlist: 252\n\
                           \playlistlength: 21\n\
                           \xfade: 0\n\
                           \state: play\n\
                           \song: 20\n\
                           \songid: 238\n\
                           \time: 158:376\n\
                           \bitrate: 192\n\
                           \audio: 44100:16:2\n\
                           \OK")
         ,("currentsong", Right "file: dir/Foo-Bar.ogg\n\
                                \Time: 60\n\
                                \Artist: Foo\n\
                                \Title: Bar\n\
                                \OK")]
         (Right . Just $ Song { sgArtist    = "Foo"
                              , sgAlbum     = ""
                              , sgTitle     = "Bar"
                              , sgFilePath  = "dir/Foo-Bar.ogg"
                              , sgGenre     = ""
                              , sgName      = ""
                              , sgComposer  = ""
                              , sgPerformer = ""
                              , sgLength    = 60
                              , sgDate      = 0
                              , sgTrack     = (0,0)
                              , sgDisc      = (0,0)
                              , sgIndex     = Nothing
                              })
         (currentSong)

testDelete0 = test_ [("delete 1", Right "OK")] (delete "" (Pos 1))

testDelete1 = test_ [("deleteid 1", Right "OK")] (delete "" (ID 1))

testDelete2 = test_ [("playlistdelete \"foo\" 1", Right "OK")] (delete "foo" (Pos 1))

testLoad = test_ [("load \"foo\"", Right "OK")] (load "foo")

testMove0 = test_ [("move 1 2", Right "OK")] (move "" (Pos 1) 2)

testMove1 = test_ [("moveid 1 2", Right "OK")] (move "" (ID 1) 2)

testMove2 = test_ [("playlistmove \"foo\" 1 2", Right "OK")] (move "foo" (Pos 1) 2)

testRm = test_ [("rm \"foo\"", Right "OK")] (rm "foo")

testRename = test_ [("rename \"foo\" \"bar\"", Right "OK")] (rename "foo" "bar")

testSave = test_ [("save \"foo\"", Right "OK")] (save "foo")

testSwap0 = test_ [("swap 1 2", Right "OK")] (swap (Pos 1) (Pos 2))

testSwap1 = test_ [("swapid 1 2", Right "OK")] (swap (ID 1) (ID 2))

testShuffle = test_ [("shuffle", Right "OK")] shuffle

--
-- Playback commands
--

testCrossfade = test_ [("crossfade 0", Right "OK")] (crossfade 0)

testPlay = test_ [("play", Right "OK")] (play Nothing)

testPause = test_ [("pause 0", Right "OK")] (pause False)

testStop = test_ [("stop", Right "OK")] stop

testNext = test_ [("next", Right "OK")] next

testPrevious = test_ [("previous", Right "OK")] previous

testRandom = test_ [("random 0", Right "OK")] (random False)

testRepeat = test_ [("repeat 0", Right "OK")] (repeat False)

testSetVolume = test_ [("setvol 10", Right "OK")] (setVolume 10)

testVolume = test_ [("volume 10", Right "OK")] (volume 10)

--
-- Miscellaneous commands
--

testClearError = test_ [("clearerror", Right "OK")] clearError

testCommands =
    test [("commands", Right "command: foo\ncommand: bar")]
         (Right ["foo", "bar"])
         commands

testNotCommands =
    test [("notcommands", Right "command: foo\ncommand: bar")]
         (Right ["foo", "bar"])
         notCommands

testTagTypes =
    test [("tagtypes", Right "tagtype: foo\ntagtype: bar")]
         (Right ["foo", "bar"])
         tagTypes

testUrlHandlers =
    test [("urlhandlers", Right "urlhandler: foo\nurlhandler: bar")]
         (Right ["foo", "bar"])
         urlHandlers

testPing = test_ [("ping", Right "OK")] ping

--
-- Extensions\/shortcuts
--
