{-
    libmpd for Haskell, a MPD client library.
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
--
module MPD (

            -- * Connections
            Connection, connect, close,

            -- * Status
            State(..), Status(..), status,
            Artist, Album, Title, Seconds, PLIndex(..),
            Song(..), currentSong,

            -- * Server control
            kill, ping, random, repeat, setVolume, getStats,

            -- * Player
            play, pause, stop, next, previous, seek,

            -- * Playlist
            add, add_, clear, delete, move, swap, getPlaylist, shuffle,

            -- * Database
            update, list, listAll, listArtists, listAlbums, listAlbum,
            findArtist, findAlbum, findTitle

           ) where


import Prelude hiding (repeat)
import Data.List (isPrefixOf, partition)
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


data PLIndex = PLNone       -- ^ No index.
             | Pos Integer  -- ^ A playlist position index.
             | ID Integer   -- ^ A playlist ID number.
               deriving Show


data State = Playing
           | Stopped
           | Paused
             deriving (Show, Eq)


data Status =
    Status { stState             :: State,
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
             -- | /TODO/: document.
             stAudio             :: (Integer,Integer,Integer) }
    deriving Show


-- | Description of a song.
--
data Song = Song { sgArtist, sgAlbum, sgTitle, sgFilePath :: String,
                   sgIndex :: PLIndex, sgLength :: Seconds }
            deriving Show



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


-- | Get the server's status.
--
status :: Connection -> IO Status
status conn = do ls <- getResponse conn "status" >>= return . kvise
                 return $ parseStatus ls
    where parseStatus xs =
              Stat { stState = maybe Stopped parseState $ lookup "state" xs,
                     stVolume = maybe 0 read $ lookup "volume" xs,
                     stRepeat = maybe False parseBool $ lookup "repeat" xs,
                     stRandom = maybe False parseBool $ lookup "random" xs,
                     stPlaylistVersion = maybe 0 read $ lookup "playlist" xs,
                     stPlaylistLength =
                         maybe 0 read $ lookup "playlistlength" xs,
                     stXFadeWidth = maybe 0 read $ lookup "xfade" xs,
                     stSongPos =
                         maybe PLNone (Pos . (1+) . read) $ lookup "song" xs,
                     stSongID = maybe PLNone (ID . read) $ lookup "songid" xs,
                     stTime = maybe (0,0) parse2 $ lookup "time" xs,
                     stBitrate = maybe 0 read $ lookup "bitrate" xs,
                     stAudio = maybe (0,0,0) parse3 $ lookup "audio" xs
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
    if stState currStatus == Stopped
        then return Nothing
        else do ls <- getResponse conn "currentsong" >>= return . kvise
                return $ if null ls then Nothing
                                    else Just (takeSongInfo ls)


-- | Kill the server. Obviously, the connection is then invalid.
--
kill :: Connection -> IO ()
kill (Conn h) = hPutStrLn h "kill" >> hClose h


-- | Check that the server is still responding.
--
ping :: Connection -> IO ()
ping conn = getResponse conn "ping" >> return ()


-- | Set random playing.
--
random :: Connection -> Bool -> IO ()
random conn x =
    getResponse conn ("random " ++ if x then "1" else "0") >> return ()


-- | Set repeating.
--
repeat :: Connection -> Bool -> IO ()
repeat conn x =
    getResponse conn ("repeat " ++ if x then "1" else "0") >> return ()


-- | Set the volume.
--
setVolume :: Connection -> Integer -> IO ()
setVolume conn x = getResponse conn ("setvol " ++ show x) >> return ()


-- | Get server statistics. /TODO/

getStats :: Connection -> IO ()
getStats _ = return ()



---------------------------------------------------------------------------
-- Player functions.
--


-- | Begin\/continue playing.
--
play :: Connection -> PLIndex -> IO ()
play conn PLNone  = getResponse conn "play" >> return ()
play conn (Pos x) = getResponse conn ("play " ++ show (x-1)) >> return ()
play conn (ID x)  = getResponse conn ("playid " ++ show x) >> return ()


-- | Pause playing.
--
pause :: Connection -> Bool -> IO ()
pause conn on =
    getResponse conn ("pause " ++ if on then "1" else "0") >> return ()


-- | Stop playing.
--
stop :: Connection -> IO ()
stop conn = getResponse conn "stop" >> return ()


-- | Play the next song.
--
next :: Connection -> IO ()
next conn = getResponse conn "next" >> return ()


-- | Play the previous song.
--
previous :: Connection -> IO ()
previous conn = getResponse conn "previous" >> return ()


-- | Seek to some point in a song. /TODO/
--
seek :: Connection -> PLIndex -> Seconds -> IO ()
seek _ _ _ = return ()



---------------------------------------------------------------------------
-- Playlist functions.
--


-- | Add a song (or a whole directory) to the playlist.
--
add :: Connection -> String -> IO [String]
add conn x = getResponse conn ("add " ++ show x) >> listAll conn (Just x)


-- | Add a song (or a whole directory) to the playlist without returning
--   what was added.
add_ :: Connection -> String -> IO ()
add_ conn x = getResponse conn ("add " ++ show x)


-- | Clear the playlist.
--
clear :: Connection -> IO ()
clear conn = getResponse conn "clear" >> return ()


-- | Remove a song from the playlist.
--
delete :: Connection -> PLIndex -> IO ()
delete _ PLNone = return ()
delete conn (Pos x) = getResponse conn ("delete " ++ show (x-1)) >> return ()
delete conn (ID  x) = getResponse conn ("deleteid " ++ show x)   >> return ()


-- | Move a song to a given position. /TODO/
--
move :: Connection -> PLIndex -> Integer -> IO ()
move _ _ _ = return ()


-- | Swap the positions of two songs. /TODO/
--
swap :: Connection -> PLIndex -> PLIndex -> IO ()
swap _ _ _ = return ()


-- | Retrieve the current playlist.
--
getPlaylist :: Connection -> IO [Song]
getPlaylist conn =
    do pl <- getResponse conn "playlistinfo" >>= return . kvise
       if null pl then return []
         else return (map takeSongInfo (splitGroups pl))


-- | Shuffle the playlist.
--
shuffle :: Connection -> IO ()
shuffle conn = getResponse conn "shuffle" >> return ()



---------------------------------------------------------------------------
-- Miscellaneous functions.
--


-- | Update the server's database.
--
update :: Connection -> [String] -> IO ()
update conn  [] = getResponse conn "update" >> return ()
update conn [x] = getResponse conn ("update " ++ x) >> return ()
update conn  xs = getResponses conn (map ("update " ++) xs) >> return ()


-- | List the directories and songs in a database directory.
--
list :: Connection -> Maybe String -> IO [Either String Song]
list conn path = do
    ls <- getResponse conn ("lsinfo " ++ path') >>= return . kvise
    -- horribly inefficient, but it works for now.
    (dirs,xs) <- return $ partition (\x -> fst x == "directory") ls
    (_,files) <- return $ partition (\x -> fst x == "playlist") xs
    return (map (Left . snd) dirs ++
            map (Right . takeSongInfo) (splitGroups files))
        where path' = maybe "" show path


-- | List the songs in a database directory recursively.
--
listAll :: Connection -> Maybe String -> IO [String]
listAll conn path = getResponse conn ("listall " ++ maybe "" show path) >>=
                    return . map snd . filter (\x -> fst x == "file") . kvise


-- | List the artists in the database.
--
listArtists :: Connection -> IO [Artist]
listArtists conn = getResponse conn "list artist" >>= return . map snd . kvise


-- | List the albums in the database, optionally matching a given
--   artist.
--
listAlbums :: Connection -> Maybe Artist -> IO [Album]
listAlbums conn artist =
    getResponse conn ("list album " ++ maybe "" show artist) >>=
    return . map snd . kvise


-- | List the songs of an album of an artist.
--
listAlbum :: Connection -> Artist -> Album -> IO [Song]
listAlbum conn artist album =
    findAlbum conn album >>= return . filter (\x -> sgArtist x == artist)


-- | Search the database.
--
find' :: Connection
     -> String      -- ^ Search type string
     -> String      -- ^ Search query
     -> IO [Song]
find' conn searchType query =
    getResponse conn ("find " ++ searchType ++ " " ++ show query) >>=
    return . map takeSongInfo . splitGroups . kvise


-- | Search the database for songs relating to an artist.
--
findArtist :: Connection -> String -> IO [Song]
findArtist conn = find' conn "artist"


-- | Search the database for songs relating to an album.
--
findAlbum :: Connection -> String -> IO [Song]
findAlbum conn = find' conn "album"


-- | Search the database for songs relating to a song title.
--
findTitle :: Connection -> String -> IO [Song]
findTitle conn = find' conn "title"



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


-- | Get the lines of the daemon's response to a list of commands.
--
getResponses :: Connection -> [String] -> IO [String]
getResponses conn cmds = getResponse conn $
    unlines $ "command_list_begin" : cmds ++ ["command_list_end"]


-- | Break up a list of strings into an assoc list, separating at
--   the first ':'.
--
kvise :: [String] -> [(String, String)]
kvise = map (\x -> let (k,v) = break (== ':') x in
                       (k,dropWhile (== ' ') $ drop 1 v))


-- | Takes a assoc list with recurring keys, and groups each cycle of
--   keys with their values together. The first key of each cycle needs
--   to be present in every cycle for it to work, but the rest don't
--   affect anything.
--
-- > splitGroups [(1,'a'),(2,'b'),(1,'c'),(2,'d')] ==
-- >     [[(1,'a'),(2,'b')],[(1,'c'),(2,'d')]]
--
splitGroups :: Eq a => [(a, b)] -> [[(a, b)]]
splitGroups [] = []
splitGroups (x:xs) = ((x:us):splitGroups vs)
    where (us,vs) = break (\y -> fst x == fst y) xs


-- |  Builds a song instance from an assoc list.
--
takeSongInfo :: [(String,String)] -> Song
takeSongInfo xs =
    Song {
          sgArtist   = maybe ""   id $ lookup "Artist" xs,
          sgAlbum    = maybe ""   id $ lookup  "Album" xs,
          sgTitle    = maybe ""   id $ lookup  "Title" xs,
          sgFilePath = maybe ""   id $ lookup   "file" xs,
          sgLength   = maybe  0 read $ lookup   "Time" xs,
          sgIndex    = maybe PLNone (ID . read) $ lookup "Id" xs
         }
