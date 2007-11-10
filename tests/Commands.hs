module Main (main) where

import Network.MPD.StringConn
import Network.MPD.Commands

import Data.Maybe
import Control.Monad

main = mapM_ (\(n, f) -> putStr (n ++ ": ") >> f) tests
    where tests = [("play", testPlay)
                  ,("stop", testStop)
                  ]

test a b c d = testMPD a b c d >>= print

testPlay = test [("play", Right "OK")] (Right ()) (return Nothing)
               (play Nothing)

testStop = test [("stop", Right "OK")] (Right ()) (return Nothing)
            stop

testLsInfo = testMPD [("lsinfo \"\"", Right "directory: Foo\ndirectory: Bar\nOK")]
                     (Right [Left "Bar", Left "Foo"])
                     (return Nothing)
                     (lsInfo "")

testAdd_ = testMPD [("add \"foo\"", Right "OK")]
                    (Right ())
                    (return Nothing)
                    (add_ "" "foo")

testAdd = testMPD [("add \"foo\"", Right "OK"), ("listall \"foo\"", Right "file: Foo\nfile: Bar\nOK")]
                  (Right ["Foo", "Bar"])
                  (return Nothing)
                  (add "" "foo")

testClear = testMPD [("playlistclear \"foo\"", Right "OK")] (Right ()) (return Nothing) (clear "foo")

testDisableOutput = testMPD
    [("disableoutput 1", Right "OK")] (Right ()) (return Nothing) (disableOutput 1)

testEnableOutput = testMPD
    [("enableoutput 1", Right "OK")] (Right ()) (return Nothing) (enableOutput 1)

testPing = testMPD [("ping", Right "OK")] (Right ()) (return Nothing) ping

testUrlHandlers = testMPD [("urlhandlers", Right "urlhandler: foo\nurlhandler: bar")]
                  (Right ["foo", "bar"])
                  (return Nothing)
                  (urlHandlers)
