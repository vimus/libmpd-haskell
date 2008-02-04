{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

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
                  ,("lsInfo", testLsInfo)
                  ,("listAllInfo", testListAllInfo)
                  ,("add", testAdd)
                  ,("add_", testAdd_)
                  ,("clear", testClear)
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

testLsInfo =
    test [("lsinfo \"\"", Right "directory: Foo\ndirectory: Bar\nOK")]
         (Right [Left "Bar", Left "Foo"])
         (lsInfo "")

testListAllInfo =
    test [("listallinfo \"\"", Right "directory: Foo\ndirectory: Bar\nOK")]
         (Right [Left "Bar", Left "Foo"])
         (listAllInfo "")

--
-- Playlist commands
--

testAdd =
    test [("add \"foo\"", Right "OK"),
          ("listall \"foo\"", Right "file: Foo\nfile: Bar\nOK")]
         (Right ["Foo", "Bar"])
         (add "" "foo")

testAdd_ = test_ [("add \"foo\"", Right "OK")] (add_ "" "foo")

testClear = test_ [("playlistclear \"foo\"", Right "OK")] (clear "foo")

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
