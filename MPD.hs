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
            Connection, connect,

            -- * Status
            State(..), Status(..), Stats(..),
            Artist, Album, Title, Seconds, PLIndex(..),
            Song(..), Count(..),

            -- * Admin commands
            disableoutput, enableoutput, kill, outputs, update,

            -- * Database commands
            findArtist, findAlbum, findTitle,
            list, listAll, listArtists, listAlbums, listAlbum,

            -- * Playlist commands
            add, add_, addid, clear, currentSong, delete, load, move, rm, save,
            getPlaylist, shuffle, swap,

            -- * Playback commands
            crossfade, next, pause, play, previous, random, repeat, seek,
            setVolume, stop,

            -- * Miscellaneous commands
            clearerror, close, commands, notcommands, password, ping, stats,
            status, tagtypes, count, urlhandlers

           ) where


import Control.Monad (liftM)
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
newtype Connection = Conn Handle


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
             stVolume            :: Int,
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
             stBitrate           :: Int,
             -- | MPD can fade between tracks. This is the time it takes to
             --   do so.
             stXFadeWidth        :: Seconds,
             -- | (samplerate, bits, channels)
             stAudio             :: (Int,Int,Int),
             -- | Job id of currently running update.
             stUpdatingDb        :: Integer,
             -- | Last error message (if any)
             stError             :: String }
    deriving Show

-- | Container for database statistics.
data Stats =
    Stats { stsArtists    :: Integer -- ^ Number of artists.
          , stsAlbums     :: Integer -- ^ Number of albums.
          , stsSongs      :: Integer -- ^ Number of songs.
          , stsUptime     :: Seconds -- ^ Daemon uptime in seconds.
          , stsPlaytime   :: Seconds -- ^ Time length of music played.
          , stsDbPlaytime :: Seconds -- ^ Sum of all song times in db.
          -- XXX should use proper data type here
          , stsDbUpdate   :: Integer -- ^ Last db update in UNIX time.
          }
    deriving Show

-- | Description of a song.
--
data Song = Song { sgArtist, sgAlbum, sgTitle, sgFilePath :: String,
                   sgIndex :: PLIndex, sgLength :: Seconds }
            deriving Show

data Count = Count { cSongs :: Integer, cPlaytime :: Seconds }
    deriving Show


-- | Represents an output device.
data Device =
    Device { dOutputID      :: Int
           , dOutputName    :: String
           , dOutputEnabled :: Bool }
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
    conn <- liftM Conn . connectTo host $ PortNumber port
    mpd <- checkConn conn
    if mpd then return conn
           else close conn >> fail ("no MPD at " ++ host ++ ":" ++ show port)


-- | Check that a MPD daemon is at the other end of a connection.
--
checkConn :: Connection -> IO Bool
checkConn (Conn h) = liftM (isPrefixOf "OK MPD") (hGetLine h)


--
-- Admin commands
--

-- | Turn off an output device.
disableoutput :: Connection -> Int -> IO ()
disableoutput conn devid =
    getResponse_ conn ("disableoutput " ++ show devid)

-- | Turn on an output device.
enableoutput :: Connection -> Int -> IO ()
enableoutput conn devid =
    getResponse_ conn ("enableoutput " ++ show devid)

-- | Kill the server. Obviously, the connection is then invalid.
--
kill :: Connection -> IO ()
kill (Conn h) = hPutStrLn h "kill" >> hClose h

-- | Retrieve information for all output devices.
outputs :: Connection -> IO [Device]
outputs conn = liftM (map takeDevInfo . splitGroups . kvise)
    (getResponse conn "outputs")

-- | Update the server's database.
--
update :: Connection -> [String] -> IO ()
update conn  [] = getResponse_ conn "update"
update conn [x] = getResponse_ conn ("update " ++ x)
update conn  xs = getResponses conn (map ("update " ++) xs) >> return ()

--
-- Database commands
--

-- | List the directories and songs in a database directory.
--
list :: Connection -> Maybe String -> IO [Either String Song]
list conn path = do
    ls <- liftM kvise (getResponse conn ("lsinfo " ++ path'))
    -- horribly inefficient, but it works for now.
    let (dirs,xs) = partition ((== "directory") . fst) ls
        (_,files) = partition ((== "playlist") . fst) xs
    return (map (Left . snd) dirs ++
            map (Right . takeSongInfo) (splitGroups files))
        where path' = maybe "" show path


-- | List the songs in a database directory recursively.
--
listAll :: Connection -> Maybe String -> IO [String]
listAll conn path = liftM (map snd . filter ((== "file") . fst) . kvise)
                          (getResponse conn ("listall " ++ maybe "" show path))


-- | List the artists in the database.
--
listArtists :: Connection -> IO [Artist]
listArtists conn = liftM (map snd . kvise) (getResponse conn "list artist")


-- | List the albums in the database, optionally matching a given
--   artist.
--
listAlbums :: Connection -> Maybe Artist -> IO [Album]
listAlbums conn artist =
    liftM (map snd . kvise)
          (getResponse conn ("list album " ++ maybe "" show artist))


-- | List the songs of an album of an artist.
--
listAlbum :: Connection -> Artist -> Album -> IO [Song]
listAlbum conn artist album = liftM (filter ((== artist) . sgArtist))
    (findAlbum conn album)


-- | Search the database.
--
find :: Connection
     -> String      -- ^ Search type string (XXX add valid values)
     -> String      -- ^ Search query
     -> IO [Song]
find conn searchType query = liftM (map takeSongInfo . splitGroups . kvise)
    (getResponse conn ("find " ++ searchType ++ " " ++ show query))


-- | Search the database for songs relating to an artist.
--
findArtist :: Connection -> String -> IO [Song]
findArtist conn = find conn "artist"


-- | Search the database for songs relating to an album.
--
findAlbum :: Connection -> String -> IO [Song]
findAlbum conn = find conn "album"


-- | Search the database for songs relating to a song title.
--
findTitle :: Connection -> String -> IO [Song]
findTitle conn = find conn "title"

--
-- Playlist commands
--

-- | Like 'add', but returns a playlist id.
addid :: Connection -> String -> IO Integer
addid conn x =
    liftM (read . snd . head . kvise) (getResponse conn ("addid " ++ show x))

-- | Add a song (or a whole directory) to the playlist.
--
add :: Connection -> String -> IO [String]
add conn x = getResponse conn ("add " ++ show x) >> listAll conn (Just x)


-- | Add a song (or a whole directory) to the playlist without returning
--   what was added.
add_ :: Connection -> String -> IO ()
add_ conn x = getResponse_ conn ("add " ++ show x)


-- | Clear the playlist.
--
clear :: Connection -> IO ()
clear conn = getResponse_ conn "clear"


-- | Remove a song from the playlist.
--
delete :: Connection -> PLIndex -> IO ()
delete _ PLNone = return ()
delete conn (Pos x) = getResponse conn ("delete " ++ show (x-1)) >> return ()
delete conn (ID  x) = getResponse conn ("deleteid " ++ show x)   >> return ()

-- | Load an existing playlist.
load :: Connection -> String -> IO ()
load conn plname = getResponse_ conn ("load " ++ show plname)


-- | Move a song to a given position.
--
move :: Connection -> PLIndex -> Integer -> IO ()
move _ PLNone _ = return ()
move conn (Pos from) to =
    getResponse_ conn ("move " ++ show (from - 1) ++ " " ++ show to)
move conn (ID from) to =
    getResponse_ conn ("moveid " ++ show from ++ " " ++ show to)

-- | Delete existing playlist.
rm :: Connection -> String -> IO ()
rm conn plname = getResponse_ conn ("rm " ++ show plname)

-- | Save the current playlist.
save :: Connection -> String -> IO ()
save conn plname = getResponse_ conn ("save " ++ show plname)


-- | Swap the positions of two songs.
--
swap :: Connection -> PLIndex -> PLIndex -> IO ()
swap conn (Pos x) (Pos y) =
    getResponse_ conn ("move " ++ show (x - 1) ++ " " ++ show (y - 1))
swap conn (ID x) (ID y) =
    getResponse_ conn ("moveid " ++ show x ++ " " ++ show y)
swap _ _ _ = return ()

-- | Shuffle the playlist.
--
shuffle :: Connection -> IO ()
shuffle conn = getResponse_ conn "shuffle"

-- | Retrieve the current playlist.
--
getPlaylist :: Connection -> IO [Song]
getPlaylist conn = do
    pl <- liftM kvise (getResponse conn "playlistinfo")
    return $ if null pl then [] else map takeSongInfo (splitGroups pl)

-- | Get the currently playing song.
currentSong :: Connection -> IO (Maybe Song)
currentSong conn = do
    currStatus <- status conn
    if stState currStatus == Stopped
        then return Nothing
        else do ls <- liftM kvise (getResponse conn "currentsong")
                return $ if null ls then Nothing
                                    else Just (takeSongInfo ls)

--
-- Playback commands
--

-- | Set crossfading between songs.
crossfade :: Connection -> Seconds -> IO ()
crossfade conn xfade = getResponse_ conn ("crossfade " ++ show xfade)

-- | Begin\/continue playing.
--
play :: Connection -> PLIndex -> IO ()
play conn PLNone  = getResponse_ conn "play"
play conn (Pos x) = getResponse_ conn ("play " ++ show (x-1))
play conn (ID x)  = getResponse_ conn ("playid " ++ show x)


-- | Pause playing.
--
pause :: Connection -> Bool -> IO ()
pause conn on =
    getResponse_ conn ("pause " ++ if on then "1" else "0")


-- | Stop playing.
--
stop :: Connection -> IO ()
stop conn = getResponse_ conn "stop"


-- | Play the next song.
--
next :: Connection -> IO ()
next conn = getResponse_ conn "next"


-- | Play the previous song.
--
previous :: Connection -> IO ()
previous conn = getResponse_ conn "previous"

-- | Seek to some point in a song.
-- Seeks in current song if no position is given.
seek :: Connection -> PLIndex -> Seconds -> IO ()
seek conn (Pos x) time =
    getResponse_ conn ("seek " ++ show (x - 1) ++ " " ++ show time)
seek conn (ID x) time =
    getResponse_ conn ("seekid " ++ show x ++ " " ++ show time)
seek conn PLNone time = do
    st <- status conn
    if stState st == Stopped
        then return ()
        else seek conn (stSongID st) time

-- | Set random playing.
--
random :: Connection -> Bool -> IO ()
random conn x =
    getResponse_ conn ("random " ++ if x then "1" else "0")


-- | Set repeating.
--
repeat :: Connection -> Bool -> IO ()
repeat conn x =
    getResponse_ conn ("repeat " ++ if x then "1" else "0")


-- | Set the volume.
--
setVolume :: Connection -> Int -> IO ()
setVolume conn x = getResponse_ conn ("setvol " ++ show x)

--
-- Miscellaneous commands
--

-- | Clear the current error message in status.
clearerror :: Connection -> IO ()
clearerror (Conn h) = hPutStrLn h "clearerror" >> hClose h

-- | Close a MPD connection.
--
close :: Connection -> IO ()
close (Conn h) = hPutStrLn h "close" >> hClose h

-- | Retrieve a list of available commands.
commands :: Connection -> IO [String]
commands conn = liftM (map snd . kvise) (getResponse conn "commands")

-- | Retrieve a list of unavailable commands.
notcommands :: Connection -> IO [String]
notcommands conn = liftM (map snd . kvise) (getResponse conn "notcommands")

-- | Send password to server to authenticate session.
-- Password is sent as plain text.
password :: Connection -> String -> IO ()
password conn passw = getResponse_ conn passw

-- | Get the server's status.
status :: Connection -> IO Status
status conn = liftM (parseStatus . kvise) (getResponse conn "status")
    where parseStatus xs =
              Status { stState = maybe Stopped parseState $ lookup "state" xs,
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
                     stTime = maybe (0,0) parseTime $ lookup "time" xs,
                     stBitrate = maybe 0 read $ lookup "bitrate" xs,
                     stAudio = maybe (0,0,0) parseAudio $ lookup "audio" xs,
                     stUpdatingDb = maybe 0 read $ lookup "updating_db" xs,
                     stError = maybe "" id $ lookup "error" xs
                   }
          parseState x = case x of "play"  -> Playing
                                   "stop"  -> Stopped
                                   "pause" -> Paused
                                   _       -> Stopped
          parseTime  x = let (y,_:z) = break (== ':') x in (read y, read z)
          parseAudio x =
              let (u,_:u') = break (== ':') x; (v,_:w) = break (== ':') u' in
                  (read u, read v, read w)


-- | Check that the server is still responding.
--
ping :: Connection -> IO ()
ping conn = getResponse_ conn "ping"


-- | Get server statistics.
stats :: Connection -> IO Stats
stats conn = liftM (parseStats . kvise) (getResponse conn "stats")
    where parseStats xs =
                Stats { stsArtists = maybe 0 read $ lookup "artists" xs,
                        stsAlbums = maybe 0 read $ lookup "albums" xs,
                        stsSongs = maybe 0 read $ lookup "songs" xs,
                        stsUptime = maybe 0 read $ lookup "uptime" xs,
                        stsPlaytime = maybe 0 read $ lookup "playtime" xs,
                        stsDbPlaytime = maybe 0 read $ lookup "db_playtime" xs,
                        stsDbUpdate = maybe 0 read $ lookup "db_update" xs }

-- | Retrieve a list of available song metadata.
tagtypes :: Connection -> IO [String]
tagtypes conn = liftM (map snd . kvise) (getResponse conn "tagtypes")

-- | Retrieve some stats on specific database entries.
count :: Connection
      -> String -- ^ Count type string (see tagtypes)
      -> String -- ^ Count query
      -> IO Count
count conn countType query = liftM (takeCountInfo . kvise)
    (getResponse conn ("count " ++ countType ++ " " ++ show query))

-- | Retrieve a list of supported urlhandlers.
urlhandlers :: Connection -> IO [String]
urlhandlers conn = liftM (map snd . kvise) (getResponse conn "urlhandlers")

---------------------------------------------------------------------------
-- Miscellaneous functions.
--

-- | Run getResponse but discard the response.
getResponse_ :: Connection -> String -> IO ()
getResponse_ c x = getResponse c x >> return ()

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
kvise = map f
    where f x = let (k,v) = break (== ':') x in
                (k,dropWhile (== ' ') $ drop 1 v)


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

-- | Builds a count instance from an assoc. list.
takeCountInfo :: [(String, String)] -> Count
takeCountInfo xs =
    Count {
        cSongs    = maybe 0 read $ lookup "songs" xs,
        cPlaytime = maybe 0 read $ lookup "playtime" xs
        }

-- | Builds a device instance from an assoc. list.
takeDevInfo :: [(String, String)] -> Device
takeDevInfo xs =
    Device {
        dOutputID      = maybe 0 read $ lookup "outputid" xs,
        dOutputName    = maybe "" id $ lookup "outputname" xs,
        dOutputEnabled = maybe False parseBool $ lookup "outputenabled" xs
        }

-- Parse a boolean response value.
parseBool :: String -> Bool
parseBool x = if x == "0" then False else True
