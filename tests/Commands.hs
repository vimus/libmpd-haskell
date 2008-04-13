{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- |
-- This module provides a way of verifying that the interface to the MPD
-- commands is correct. It does so by capturing the data flow between the
-- command and a dummy socket, checking the captured data against a set of
-- predefined values that are known to be correct. Of course, this does not
-- verify that the external behaviour is correct, it's simply a way of
-- catching silly mistakes and subtle bugs in the interface itself, without
-- having to actually send any requests to a real server.

module Commands (main) where

import Displayable
import Network.MPD.Commands
import Network.MPD.Core (Response, MPDError(..))
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
                  ,("find / complex query", testFindComplex)
                  ,("list(Nothing)", testListNothing)
                  ,("list(Just)", testListJust)
                  ,("listAll", testListAll)
                  ,("lsInfo", testLsInfo)
                  ,("listAllInfo", testListAllInfo)
                  ,("search", testSearch)
                  ,("count", testCount)
                  ,("add", testAdd)
                  ,("add_", testAdd_)
                  ,("add_ / playlist", testAdd_pl)
                  ,("addId", testAddId)
                  ,("clear / playlist", testClearPlaylist)
                  ,("clear / current", testClearCurrent)
                  ,("plChangesPosId 0", testPlChangesPosId_0)
                  ,("plChangesPosId 1", testPlChangesPosId_1)
                  ,("plChangesPosId wierd", testPlChangesPosId_Wierd)
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
                  ,("playlistInfo0", testPlaylistInfo0)
                  ,("playlistInfo / pos", testPlaylistInfoPos)
                  ,("playlistInfo / id", testPlaylistInfoId)
                  ,("listPlaylistInfo", testListPlaylistInfo)
                  ,("listPlaylist", testListPlaylist)
                  ,("playlist", testPlaylist)
                  ,("plchanges", testPlChanges)
                  ,("playlistFind", testPlaylistFind)
                  ,("playlistSearch", testPlaylistSearch)
                  ,("crossfade", testCrossfade)
                  ,("play", testPlay)
                  ,("play / pos", testPlayPos)
                  ,("play / id", testPlayId)
                  ,("pause", testPause)
                  ,("stop", testStop)
                  ,("next", testNext)
                  ,("previous", testPrevious)
                  ,("seek / pos", testSeekPos)
                  ,("seek / id", testSeekId)
                  ,("seek / current", testSeekCur)
                  ,("random", testRandom)
                  ,("repeat", testRepeat)
                  ,("setVolume", testSetVolume)
                  ,("volume", testVolume)
                  ,("clearError", testClearError)
                  ,("commands", testCommands)
                  ,("notCommands", testNotCommands)
                  ,("tagTypes", testTagTypes)
                  ,("urlHandlers", testUrlHandlers)
                  ,("password", testPassword)
                  ,("ping", testPing)
                  ,("stats", testStats)
                  ,("updateId0", testUpdateId0)
                  ,("updateId1", testUpdateId1)
                  ,("toggle / stop", testToggleStop)
                  ,("toggle / play", testTogglePlay)
                  ,("toggle / pause", testTogglePause)
                  ,("addMany0", testAddMany0)
                  ,("addMany1", testAddMany1)
                  ,("deleteMany1", testDeleteMany1)
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
    test [("outputs", Right resp)] (Right [obj1, obj2]) outputs
    where obj1 = empty { dOutputName = "SoundCard0", dOutputEnabled = True }
          obj2 = empty { dOutputName = "SoundCard1", dOutputID = 1 }
          resp = concatMap display [obj1, obj2] ++ "OK"

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
    test [("find Artist \"Foo\"", Right resp)] (Right [obj])
    (find (Query Artist "Foo"))
    where obj = empty { sgArtist = "Foo", sgTitle = "Bar"
                      , sgFilePath = "dir/Foo-Bar.ogg", sgLength = 60 }
          resp = display obj ++ "OK"

testFindComplex =
    test [("find Artist \"Foo\" Album \"Bar\"", Right resp)] (Right [obj])
    (find $ MultiQuery [Query Artist "Foo", Query Album "Bar"])
    where obj = empty { sgFilePath = "dir/Foo/Bar/Baz.ogg", sgArtist = "Foo"
                      , sgAlbum = "Bar", sgTitle = "Baz" }
          resp = display obj ++ "OK"

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
    test [("search Artist \"oo\"", Right resp)] (Right [obj])
         (search (Query Artist "oo"))
    where obj = empty { sgArtist = "Foo", sgTitle = "Bar"
                      , sgFilePath = "dir/Foo-Bar.ogg", sgLength = 60 }
          resp = display obj ++ "OK"

testCount =
    test [("count Title \"Foo\"", Right resp)] (Right obj)
         (count (Query Title "Foo"))
    where obj = Count 1 60
          resp = display obj ++ "OK"

--
-- Playlist commands
--

testAdd =
    test [("add \"foo\"", Right "OK"),
          ("listall \"foo\"", Right "file: Foo\nfile: Bar\nOK")]
         (Right ["Foo", "Bar"])
         (add "" "foo")

testAdd_ = test_ [("add \"foo\"", Right "OK")] (add_ "" "foo")

testAdd_pl = test_ [("playlistadd \"foo\" \"bar\"", Right "OK")]
             (add_ "foo" "bar")

testAddId =
    test [("addid \"dir/Foo-Bar.ogg\"", Right "Id: 20\nOK")]
         (Right 20)
         (addId "dir/Foo-Bar.ogg")

testClearPlaylist = test_ [("playlistclear \"foo\"", Right "OK")]
                    (clear "foo")

testClearCurrent = test_ [("clear", Right "OK")] (clear "")

testPlChangesPosId_0 =
    test [("plchangesposid 10", Right "OK")]
         (Right [])
         (plChangesPosId 10)

testPlChangesPosId_1 =
    test [("plchangesposid 10", Right "cpos: 0\nId: 20\nOK")]
         (Right [(Pos 0, ID 20)])
         (plChangesPosId 10)

testPlChangesPosId_Wierd =
    test [("plchangesposid 10", Right "cpos: foo\nId: bar\nOK")]
         (Left $ Unexpected "[(\"cpos\",\"foo\"),(\"Id\",\"bar\")]")
         (plChangesPosId 10)

testCurrentSongStopped =
    test [("status", Right resp)] (Right Nothing)
         (currentSong)
    where obj = empty { stState = Stopped, stPlaylistVersion = 253 }
          resp = display obj ++ "OK"

testCurrentSongPlaying =
    test [("status", Right resp2)
         ,("currentsong", Right resp1)]
         (Right $ Just song)
         (currentSong)
    where song = empty { sgArtist = "Foo", sgTitle = "Bar"
                       , sgFilePath = "dir/Foo-Bar.ogg", sgLength = 60 }
          resp1 = display song

          estatus = empty { stVolume = 80, stPlaylistVersion = 252
                          , stPlaylistLength = 21, stSongPos = Just (Pos 20)
                          , stSongID = Just (ID 238), stTime = (158, 376)
                          , stBitrate = 192, stAudio = (44100, 16, 2)
                          , stState = Playing }
          resp2 = display estatus ++ "OK"

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

testPlaylistInfo0 = test [("playlistinfo", Right resp)] (Right [obj])
                    (playlistInfo Nothing)
    where obj = empty { sgFilePath = "dir/Foo-Bar.ogg", sgLength = 60
                      , sgArtist = "Foo", sgTitle = "Bar" }
          resp = display obj ++ "OK"

testPlaylistInfoPos = test [("playlistinfo 1", Right resp)] (Right [obj])
                      (playlistInfo . Just $ Pos 1)
    where obj = empty { sgFilePath = "dir/Foo-Bar.ogg", sgLength = 60
                      , sgArtist = "Foo", sgTitle = "Bar" }
          resp = display obj ++ "OK"

testPlaylistInfoId = test [("playlistid 1", Right resp)] (Right [obj])
                     (playlistInfo . Just $ ID 1)
    where obj = empty { sgFilePath = "dir/Foo-Bar.ogg", sgLength = 60
                      , sgArtist = "Foo", sgTitle = "Bar" }
          resp = display obj ++ "OK"

testListPlaylistInfo = test [("listplaylistinfo \"foo\"", Right resp)]
                       (Right [obj])
                       (listPlaylistInfo "foo")
    where obj = empty { sgFilePath = "dir/Foo-Bar.ogg", sgLength = 60
                      , sgArtist = "Foo", sgTitle = "Bar" }
          resp = display obj ++ "OK"

testListPlaylist = test [("listplaylist \"foo\""
                         ,Right "file: dir/Foo-bar.ogg\n\
                                \file: dir/Quux-quuz.ogg\n\
                                \OK")]
                   (Right ["dir/Foo-bar.ogg", "dir/Quux-quuz.ogg"])
                   (listPlaylist "foo")

testPlaylist = test [("playlist"
                     ,Right "1:Foo.ogg\n\
                            \2:Bar.ogg\n\
                            \OK")]
               (Right [(Pos 1, "Foo.ogg")
                      ,(Pos 2, "Bar.ogg")])
               playlist

testPlChanges = test [("plchanges 0", Right resp)] (Right [obj]) (plChanges 0)
    where obj = empty { sgArtist = "Foo", sgTitle = "Bar"
                      , sgFilePath = "foo/bar.ogg" }
          resp = display obj ++ "OK"

testPlaylistFind = test [("playlistfind Artist \"Foo\"", Right resp)]
                   (Right [obj])
                   (playlistFind $ Query Artist "Foo")
    where obj = empty { sgFilePath = "dir/Foo/Bar.ogg", sgArtist = "Foo" }
          resp = display obj ++ "OK"

testPlaylistSearch = test [("playlistsearch Artist \"Foo\"", Right resp)]
                     (Right [obj])
                     (playlistSearch $ Query Artist "Foo")
    where obj = empty { sgFilePath = "dir/Foo/Bar.ogg", sgArtist = "Foo" }
          resp = display obj ++ "OK"

--
-- Playback commands
--

testCrossfade = test_ [("crossfade 0", Right "OK")] (crossfade 0)

testPlay = test_ [("play", Right "OK")] (play Nothing)

testPlayPos = test_ [("play 1", Right "OK")] (play . Just $ Pos 1)

testPlayId = test_ [("playid 1", Right "OK")] (play . Just $ ID 1)

testPause = test_ [("pause 0", Right "OK")] (pause False)

testStop = test_ [("stop", Right "OK")] stop

testNext = test_ [("next", Right "OK")] next

testPrevious = test_ [("previous", Right "OK")] previous

testSeekPos = test_ [("seek 1 10", Right "OK")] (seek (Just $ Pos 1) 10)

testSeekId = test_ [("seekid 1 10", Right "OK")] (seek (Just $ ID 1) 10)

testSeekCur = test_ [("status", Right resp)
                    ,("seekid 1 10", Right "OK")]
              (seek Nothing 10)
    where obj = empty { stState = Playing, stSongID = Just (ID 1) }
          resp = display obj ++ "OK"

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

testPassword = test_ [("password foo", Right "OK")] (password "foo")

testPing = test_ [("ping", Right "OK")] ping

testStats = test [("stats", Right resp)] (Right obj) stats
    where obj = empty { stsArtists = 1, stsAlbums = 1, stsSongs =  1
                      , stsUptime = 100, stsPlaytime = 100, stsDbUpdate = 10
                      , stsDbPlaytime = 100 }
          resp = display obj ++ "OK"

--
-- Extensions\/shortcuts
--

testUpdateId0 = test [("update", Right "updating_db: 1")]
                (Right 1)
                (updateId [])

testUpdateId1 = test [("update \"foo\"", Right "updating_db: 1")]
                (Right 1)
                (updateId ["foo"])

testTogglePlay = test_
               [("status", Right resp)
               ,("pause 1", Right "OK")]
               toggle
    where resp = display empty { stState = Playing }

testToggleStop = test_
                [("status", Right resp)
                ,("play", Right "OK")]
                toggle
    where resp = display empty { stState = Stopped }

testTogglePause = test_
                [("status", Right resp)
                ,("play", Right "OK")]
                toggle
    where resp = display empty { stState = Paused }

testAddMany0 = test_ [("add \"bar\"", Right "OK")]
               (addMany "" ["bar"])

testAddMany1 = test_ [("playlistadd \"foo\" \"bar\"", Right "OK")]
               (addMany "foo" ["bar"])

testDeleteMany1 = test_ [("playlistdelete \"foo\" 1", Right "OK")]
                  (deleteMany "foo" [Pos 1])
