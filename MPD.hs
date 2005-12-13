{-
    libmpd for Haskell
    Copyright (C) 2005  Ben Sinclair <bsinclai@turing.une.edu.au>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}


-- | MPD client library.
module MPD (-- * Connections
            Connection, connect, close,

            -- * Status
            State(..), Status(..), status,
            Artist, Album, Title, Seconds, PLIndex(..),
            Song(..), currentSong,

            -- * Server control
            kill, update, ping, random, repeat, setvol, stats,

            -- * Player
            play, pause, stop, next, previous, seek,

            -- * Playlist
            add, clear, delete, move, playlist, shuffle, swap,

            -- * Database
            list, listall, find, listdir
           ) where


import Prelude hiding (repeat)
import Data.List (isPrefixOf)
import Data.Maybe
import Network
import System.IO



---------------------------------------------------------------------------
-- Data Types
--


-- | A connection to a MPD.
--
data Connection = Conn Handle


type Artist  = String
type Album   = String
type Title   = String
type Seconds = Integer


data PLIndex = Pos Integer  -- ^ A playlist position index.
             | ID Integer   -- ^ A playlist ID number.
             | PLNone       -- ^ No index.
               deriving Show

-- | The play state of a MPD.
--
data State = Playing
           | Stopped
           | Paused
             deriving (Show, Eq)


-- | State of a MPD.
--
data Status =
    Stat { stState             :: State,
           -- | A percentage.
           stVolume            :: Integer,
           stRepeat, stRandom  :: Bool,
           -- | This value gets incremented by the server every time the
           --   playlist changes.
           stPlaylistVersion   :: Integer,
           stPlaylistLength    :: Integer,
           -- | Current song's position in the playlist (starting from 1).
           stSongPos           :: PLIndex,
           -- | Each song in the playlist has an identifier to more
           --   robustly identify it.
           stSongID            :: PLIndex,
           -- | (Seconds played, song length in seconds).
           stTime              :: (Seconds,Seconds),
           -- | Bitrate of playing song in kilobytes per second.
           stBitrate           :: Integer,
           -- | MPD can fade between tracks. This is the time it takes to
           --   do so.
           stXFadeWidth        :: Seconds,
           -- | TODO: document.
           stAudio             :: (Integer,Integer,Integer) }
    deriving Show


-- | Description of a song.
--
data Song = Song { sgArtist, sgAlbum, sgTitle :: String }



---------------------------------------------------------------------------
-- Basic connection functions
--


-- | Create a MPD connection.
--
connect :: String      -- ^ Hostname.
        -> PortNumber  -- ^ Port number.
        -> IO Connection
connect host port = withSocketsDo $ do
    conn <- connectTo host (PortNumber port) >>= return . Conn
    mpd <- checkConn conn
    if mpd then return conn
           else close conn >> fail ("no MPD at " ++ host ++ ":" ++ show port)


-- | Close a MPD connection.
--
close :: Connection -> IO ()
close (Conn h) = hPutStrLn h "close" >> hClose h


-- | Check that a MPD daemon is at the other end of a connection.
--
checkConn :: Connection -> IO Bool
checkConn (Conn h) = hGetLine h >>= return . isPrefixOf "OK MPD"



---------------------------------------------------------------------------
-- MPD status functions
--


-- | Get the current status of the MPD daemon.
--
status :: Connection -> IO Status
status conn = do ls <- getResponse conn "status" >>= return . kvise
                 return $ parseStatus ls
    where parseStatus xs =
              Stat { stState  = maybe Stopped parseState $ lookup  "state" xs,
                     stVolume = maybe 0 read           $ lookup   "volume" xs,
                     stRepeat = maybe False parseBool  $ lookup   "repeat" xs,
                     stRandom = maybe False parseBool  $ lookup   "random" xs,
                     stPlaylistVersion = maybe 0 read  $ lookup "playlist" xs,
                     stPlaylistLength = maybe 0 read   $
                                        lookup "playlistlength" xs,
                     stXFadeWidth = maybe 0 read       $ lookup    "xfade" xs,
                     stSongPos =
                         maybe PLNone (Pos . (1+) . read) $ lookup "song" xs,
                     stSongID  = maybe PLNone (ID . read) $
                                 lookup "songid" xs,
                     stTime    = maybe (0,0) parse2    $ lookup     "time" xs,
                     stBitrate = maybe 0 read          $ lookup  "bitrate" xs,
                     stAudio   = maybe (0,0,0) parse3  $ lookup    "audio" xs
                   }
          parseState x = case x of "play"  -> Playing
                                   "stop"  -> Stopped
                                   "pause" -> Paused
                                   _       -> Stopped
          parseBool  x = if x == "0" then False else True
          parse2     x = let (y,_:z) = break (== ':') x in (read y, read z)
          parse3     x =
              let (u,_:u') = break (== ':') x; (v,_:w) = break (== ':') u' in
                  (read u, read v, read w)


-- | Get the currently playing song.
--
currentSong :: Connection -> IO (Maybe Song)
currentSong conn = do
    currStatus <- status conn
    if stState currStatus == Stopped then return Nothing
        else do ls  <- getResponse conn "currentsong" >>= return . kvise
                return $ if null ls then
                             Nothing
                         else
                             Just $ Song (fromJust $ lookup "Artist" ls)
                                         (fromJust $ lookup "Album"  ls)
                                         (fromJust $ lookup "Title"  ls)


-- | Kill the MPD. Obviously, the connection is then invalid.
--
kill :: Connection -> IO ()
kill (Conn h) = hPutStrLn h "kill" >> hClose h


-- | Update the MPD database.
--
update :: Connection -> [String] -> IO ()
update conn  [] = getResponse conn "update" >> return ()
update conn [x] = getResponse conn ("update " ++ x) >> return ()
update conn  xs = getResponses conn (map ("update " ++) xs) >> return ()


-- | Check that the MPD is still responding.
--
ping :: Connection -> IO ()
ping conn = getResponse conn "ping" >> return ()


-- | Set random playing. TODO: Nothing case.
--
random :: Connection -> Maybe Bool -> IO ()
random conn (Just True)  = getResponse conn "random 1" >> return ()
random conn (Just False) = getResponse conn "random 0" >> return ()
random _    Nothing      = return () -- TODO: Toggle random.


-- | Set repeating. TODO: Nothing case.
--
repeat :: Connection -> Maybe Bool -> IO ()
repeat conn (Just True)  = getResponse conn "repeat 1" >> return ()
repeat conn (Just False) = getResponse conn "repeat 0" >> return ()
repeat _    Nothing      = return () -- TODO: Toggle repeat.


-- | Set the volume. TODO
--
setvol :: Connection -> Integer -> IO ()
setvol _ _ = return ()


-- | Get MPD statistics. TODO

stats :: Connection -> IO ()
stats _ = return ()



---------------------------------------------------------------------------
-- Player functions.
--


-- | Begin\/continue playing.
play :: Connection -> PLIndex -> IO ()
play conn PLNone  = getResponse conn "play" >> return ()
play conn (Pos x) = getResponse conn ("play " ++ show (x-1)) >> return ()
play conn (ID x)  = getResponse conn ("playid " ++ show x) >> return ()


-- | Pause playing.
pause :: Connection -> Bool -> IO ()
pause conn on =
    getResponse conn ("pause " ++ if on then "1" else "0") >> return ()


-- | Stop playing.
stop :: Connection -> IO ()
stop conn = getResponse conn "stop" >> return ()


-- | Play the next song.
next :: Connection -> IO ()
next conn = getResponse conn "next" >> return ()


-- | Play the previous song.
previous :: Connection -> IO ()
previous conn = getResponse conn "previous" >> return ()


-- | Seek to some point in a song. TODO
seek :: Connection -> IO ()
seek _ = return ()



---------------------------------------------------------------------------
-- Playlist functions.
--


-- | Add songs (or directories of songs) to the playlist. TODO
--
add :: Connection -> String -> IO ()
add _ _ = return ()


-- | Clear the playlist.
--
clear :: Connection -> IO ()
clear conn = getResponse conn "clear" >> return ()


-- |  TODO
--
delete :: Connection -> PLIndex -> IO ()
delete _ _ = return ()


-- |  TODO
--
move :: Connection -> PLIndex -> Integer -> IO ()
move _ _ _ = return ()


-- |  TODO
--
playlist :: Connection -> IO [String]
playlist _ = return []


-- | Shuffle the playlist. TODO
--
shuffle :: Connection -> IO ()
shuffle _ = return ()


-- |  TODO
--
swap :: Connection -> PLIndex -> PLIndex -> IO ()
swap _ _ _ = return ()



---------------------------------------------------------------------------
-- Miscellaneous functions.
--


-- |  TODO
--
list :: Connection -> IO ()
list _ = return ()


-- |  TODO
--
listall :: Connection -> IO ()
listall _ = return ()


-- |  TODO
--
find :: Connection -> IO ()
find _ = return ()


-- |  TODO
--
listdir :: Connection -> String -> IO ()
listdir _ _ = return ()




---------------------------------------------------------------------------
-- Miscellaneous functions.
--


-- | Get the lines of the daemon's response to a given command.
--
getResponse :: Connection -> String -> IO [String]
getResponse (Conn h) cmd = hPutStrLn h cmd >> hFlush h >> f []
    where f acc = do
              l <- hGetLine h
              case l of
                  "OK"              -> return acc
                  ('A':'C':'K':_:e) -> fail e
                  _                 -> f (acc ++ [l])


getResponses :: Connection -> [String] -> IO [String]
getResponses conn cmds = getResponse conn $
    unlines $ ["command_list_begin"] ++ cmds ++ ["command_list_end"]


-- | Break up a list of strings into pairs of strings, separating at
--   the first ':'.
--
kvise :: [String] -> [(String, String)]
kvise = map (\x -> let (k,v) = break (== ':') x in
                       (k,dropWhile (== ' ') $ tail v))
