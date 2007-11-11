{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Network.MPD.StringConn
import Network.MPD.Commands

import Data.Maybe
import Control.Monad

main = mapM_ (\(n, f) -> f >>= \x -> putStrLn (n ++ ": " ++ x)) tests
    where tests = [("play", testPlay)
                  ,("stop", testStop)
                  ,("lsInfo", testLsInfo)
                  ,("add_", testAdd_)
                  ,("add", testAdd)
                  ,("clear", testClear)
                  ,("outputs", testOutputs)
                  ,("disableOutput", testDisableOutput)
                  ,("enableOutput", testEnableOutput)
                  ,("update0", testUpdate0)
                  ,("update1", testUpdate1)
                  ,("updateMany", testUpdateMany)
                  ,("ping", testPing)
                  ,("urlHandlers", testUrlHandlers)
                  ]

test a b c = liftM show $ testMPD a b (return Nothing) c

test_ a b = test a (Right ()) b

testPlay = test_ [("play", Right "OK")] (play Nothing)

testStop = test_ [("stop", Right "OK")] stop

testLsInfo =
    test [("lsinfo \"\"", Right "directory: Foo\ndirectory: Bar\nOK")]
         (Right [Left "Bar", Left "Foo"])
         (lsInfo "")

testAdd_ = test_ [("add \"foo\"", Right "OK")] (add_ "" "foo")

testAdd =
    test [("add \"foo\"", Right "OK"),
          ("listall \"foo\"", Right "file: Foo\nfile: Bar\nOK")]
         (Right ["Foo", "Bar"])
         (add "" "foo")

testClear = test_ [("playlistclear \"foo\"", Right "OK")] (clear "foo")

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

testDisableOutput = test_ [("disableoutput 1", Right "OK")] (disableOutput 1)

testEnableOutput = test_ [("enableoutput 1", Right "OK")] (enableOutput 1)

testUpdate0 = test_ [("update", Right "updating_db: 1\nOK")] (update [])

testUpdate1 =
    test_ [("update \"foo\"", Right "updating_db: 1\nOK")]
          (update ["foo"])

testUpdateMany =
    test_ [("command_list_begin\nupdate \"foo\"\nupdate \"bar\"\n\
           \command_list_end", Right "updating_db: 1\nOK")]
         (update ["foo","bar"])

testPing = test_ [("ping", Right "OK")] ping

testUrlHandlers =
    test [("urlhandlers", Right "urlhandler: foo\nurlhandler: bar")]
         (Right ["foo", "bar"])
         (urlHandlers)
