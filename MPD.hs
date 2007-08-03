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

-- | Module    : MPD
-- Copyright   : (c) Ben Sinclair 2005
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- MPD client library.

module MPD (
            -- * Data types
            Connection,
            State(..), Status(..), Stats(..),
            Device(..),
            Artist, Album, Title, Seconds, PLIndex(..),
            Song(..), Count(..),

            -- * Connections
            connect,

            -- * Admin commands
            disableoutput, enableoutput, kill, outputs, update,

            -- * Database commands
            -- $database
            find, list, listAll, listAllinfo, lsinfo, search, count,

            -- * Playlist commands
            -- $playlist
            add, add_, addid, clear, currentSong, delete, load, move,
            playlistinfo, listplaylist, listplaylistinfo, playlist, plchanges,
            plchangesposid, rm, rename, save, shuffle, swap,

            -- * Playback commands
            crossfade, next, pause, play, previous, random, repeat, seek,
            setVolume, volume, stop,

            -- * Miscellaneous commands
            clearerror, close, commands, notcommands, tagtypes, urlhandlers,
            password, ping, stats, status,

            -- * Extensions\/shortcuts
            addMany, crop, findArtist, findAlbum, findTitle, listArtists,
            listAlbums, listAlbum, searchArtist, searchAlbum, searchTitle,
            getPlaylist, toggle
           ) where

import Control.Monad (liftM, unless)
import Prelude hiding (repeat)
import Data.List (isPrefixOf)
import Data.Maybe
import Network
import System.IO

--
-- Data Types
--

-- | A connection to a MPD.
newtype Connection = Conn Handle

type Artist  = String
type Album   = String
type Title   = String
type Seconds = Integer

-- | Represents a song's playlist index.
data PLIndex = PLNone       -- ^ No index.
             | Pos Integer  -- ^ A playlist position index (starting from 1).
             | ID Integer   -- ^ A playlist ID number.
               deriving Show

-- | Represents the different playback states.
data State = Playing
           | Stopped
           | Paused
             deriving (Show, Eq)

-- | Container for MPD status.
data Status =
    Status { stState             :: State,
             -- | A percentage (0-100).
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
             -- | Job id of currently running update (if any).
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
          , stsDbUpdate   :: Integer -- ^ Last db update in UNIX time.
          }
    deriving Show

-- | Description of a song.
data Song = Song { sgArtist, sgAlbum, sgTitle, sgFilePath, sgGenre, sgName
                  ,sgComposer, sgPerformer :: String
                  ,sgLength :: Seconds    -- ^ length in seconds
                  ,sgDate   :: Int        -- ^ year
                  ,sgTrack  :: (Int, Int) -- ^ (track number, total tracks)
                  ,sgDisc   :: (Int, Int) -- ^ (pos. in set, total in set)
                  ,sgIndex  :: PLIndex }
            deriving Show

-- | Describes a 'count'.
data Count = Count { cSongs    :: Integer -- ^ Number of songs that matches
                                          -- a query
                   , cPlaytime :: Seconds -- ^ Total play time of matching
                                          -- songs
                   }
    deriving Show

-- | Represents an output device.
data Device =
    Device { dOutputID      :: Int    -- ^ Output's id number
           , dOutputName    :: String -- ^ Output's name as defined in the MPD
                                      -- configuration file
           , dOutputEnabled :: Bool }
    deriving Show

--
-- Basic connection functions
--

-- | Create a MPD connection.
connect :: String      -- ^ Hostname.
        -> PortNumber  -- ^ Port number.
        -> IO Connection
connect host port = withSocketsDo $ do
    conn <- liftM Conn . connectTo host $ PortNumber port
    mpd <- checkConn conn
    if mpd then return conn
           else close conn >> fail ("no MPD at " ++ host ++ ":" ++ show port)

-- | Check that a MPD daemon is at the other end of a connection.
checkConn :: Connection -> IO Bool
checkConn (Conn h) = liftM (isPrefixOf "OK MPD") (hGetLine h)

--
-- Admin commands
--

-- | Turn off an output device.
disableoutput :: Connection -> Int -> IO ()
disableoutput conn = getResponse_ conn . ("disableoutput " ++) . show

-- | Turn on an output device.
enableoutput :: Connection -> Int -> IO ()
enableoutput conn = getResponse_ conn . ("enableoutput " ++) . show

-- | Kill the server. Obviously, the connection is then invalid.
kill :: Connection -> IO ()
kill (Conn h) = hPutStrLn h "kill" >> hClose h

-- | Retrieve information for all output devices.
outputs :: Connection -> IO [Device]
outputs conn = liftM (map takeDevInfo . splitGroups . kvise)
    (getResponse conn "outputs")
    where
        takeDevInfo xs = Device {
            dOutputID      = takeNum "outputid" xs,
            dOutputName    = takeString "outputname" xs,
            dOutputEnabled = takeBool "outputenabled" xs
            }

-- | Update the server's database.
update :: Connection -> [String] -> IO ()
update conn  [] = getResponse_ conn "update"
update conn [x] = getResponse_ conn ("update " ++ x)
update conn  xs = getResponses conn (map ("update " ++) xs) >> return ()

--
-- Database commands
--
-- $database
-- All scope modifiers (i.e. metadata to match against when searching for
-- database entries with certain metadata values) may be any of the
-- values listed by 'tagtypes'.
-- Also one may use \"any\" or \"filename\".

-- | List all metadata of metadata (sic).
list :: Connection
     -> String       -- ^ Metadata to list.
     -> Maybe String -- ^ Optionally specify a scope modifier
     -> String       -- ^ Query (requires optional arg).
     -> IO [String]
list conn metaType metaQuery query = liftM takeValues (getResponse conn cmd)
    where cmd = "list " ++ metaType ++
                maybe "" (\x -> " " ++ x ++ " " ++ show query) metaQuery

-- | Non-recursivly list the contents of a database directory.
lsinfo :: Connection -> Maybe String -- ^ Optionally specify a path.
       -> IO [Either String Song]
lsinfo conn path = do
    (dirs,_,filedata) <- liftM (foldl split ([],[],[]) . kvise)
                         (getResponse conn ("lsinfo " ++ maybe "" show path))
    return (map Left dirs ++
            map (Right . takeSongInfo) (splitGroups $ reverse filedata))
    where split (ds,pls,ss) x@(k,v) | k == "directory" = (v:ds, pls, ss)
                                    | k == "playlist"  = (ds, v:pls, ss)
                                    | otherwise        = (ds, pls, x:ss)

-- | List the songs (without metadata) in a database directory recursively.
listAll :: Connection -> Maybe String -> IO [String]
listAll conn path = liftM (map snd . filter ((== "file") . fst) . kvise)
                          (getResponse conn ("listall " ++ maybe "" show path))

-- | List all information in database about all songs in a given path.
listAllinfo :: Connection -> String -> IO [Song]
listAllinfo _ _ = undefined

-- | Search the database for entries exactly matching a query.
find :: Connection
     -> String      -- ^ Scope modifier
     -> String      -- ^ Query
     -> IO [Song]
find conn searchType query = liftM takeSongs
    (getResponse conn ("find " ++ searchType ++ " " ++ show query))

-- | Search the database using case insensitive matching.
search :: Connection
       -> String -- ^ Scope modifier
       -> String -- ^ Query
       -> IO [Song]
search conn searchType query = liftM takeSongs
    (getResponse conn ("search " ++ searchType ++ " " ++ show query))

-- | Count the number of entries matching a query.
count :: Connection
      -> String -- ^ Scope modifier
      -> String -- ^ Query
      -> IO Count
count conn countType query = liftM (takeCountInfo . kvise)
    (getResponse conn ("count " ++ countType ++ " " ++ show query))
    where takeCountInfo xs = Count { cSongs    = takeNum "songs" xs,
                                     cPlaytime = takeNum "playtime" xs }

--
-- Playlist commands
--
-- $playlist
-- Unless otherwise noted all playlist commands operate on the current
-- playlist.

-- | Like 'add', but returns a playlist id.
addid :: Connection -> String -> IO Integer
addid conn x =
    liftM (read . snd . head . kvise) (getResponse conn ("addid " ++ show x))

-- | Like 'add_' but returns a list of the files added.
add :: Connection -> Maybe String -> String -> IO [String]
add conn plname x = add_ conn plname x >> listAll conn (Just x)

-- | Add a song (or a whole directory) to a playlist.
-- Adds to current if no playlist is specified.
-- Will create a new playlist if the one specified does not already exist.
add_ :: Connection
     -> Maybe String -- ^ Optionally specify a playlist to operate on
     -> String
     -> IO ()
add_ conn plname x = getResponse_ conn cmd
    where cmd  = maybe ("add " ++ path)
                       (\pl -> "playlistadd " ++ show pl ++ " " ++ path)
                       plname
          path = show x

-- | Clear a playlist. Clears current playlist if no playlist is specified.
-- If the specified playlist does not exist, it will be created.
clear :: Connection
      -> Maybe String -- ^ Optional name of a playlist to clear.
      -> IO ()
clear conn Nothing       = getResponse_ conn "clear"
clear conn (Just plname) = getResponse_ conn ("playlistclear " ++ show plname)

-- | Remove a song from a playlist.
-- If no playlist is specified, current playlist is used.
delete :: Connection
       -> Maybe String -- ^ Optionally specify a playlist to operate on
       -> PLIndex -> IO ()
delete _ _ PLNone = return ()
delete conn Nothing (Pos x) = getResponse_ conn ("delete " ++ show (x - 1))
delete conn Nothing (ID x) = getResponse_ conn ("deleteid " ++ show x)
-- XXX assume that playlistdelete expects positions and not ids.
delete conn (Just plname) (Pos x) =
    getResponse_ conn ("playlistdelete " ++ show plname ++ " " ++ show (x - 1))
delete _ _ _ = return ()

-- | Load an existing playlist.
load :: Connection -> String -> IO ()
load conn = getResponse_ conn . ("load " ++) . show

-- | Move a song to a given position.
move :: Connection
     -> Maybe String -- ^ Optionally specify a playlist to operate on
     -> PLIndex -> Integer -> IO ()
move _ _ PLNone _ = return ()
move conn Nothing (Pos from) to =
    getResponse_ conn ("move " ++ show (from - 1) ++ " " ++ show to)
move conn Nothing (ID from) to =
    getResponse_ conn ("moveid " ++ show from ++ " " ++ show to)
-- XXX assumes that playlistmove expects positions and not ids
move conn (Just plname) (Pos from) to =
    getResponse_ conn ("playlistmove " ++ show plname ++ " " ++ show (from - 1)
                       ++ " " ++ show to)
move _ _ _ _ = return ()

-- | Delete existing playlist.
rm :: Connection -> String -> IO ()
rm conn = getResponse_ conn . ("rm " ++) . show

-- | Rename an existing playlist.
rename :: Connection
       -> String -- ^ Name of playlist to be renamed
       -> String -- ^ New playlist name
       -> IO ()
rename conn plname new =
    getResponse_ conn ("rename " ++ show plname ++ " " ++ show new)

-- | Save the current playlist.
save :: Connection -> String -> IO ()
save conn = getResponse_ conn . ("save " ++) . show

-- | Swap the positions of two songs.
swap :: Connection -> PLIndex -> PLIndex -> IO ()
swap conn (Pos x) (Pos y) =
    getResponse_ conn ("swap " ++ show (x - 1) ++ " " ++ show (y - 1))
swap conn (ID x) (ID y) =
    getResponse_ conn ("swapid " ++ show x ++ " " ++ show y)
swap _ _ _ = return ()

-- | Shuffle the playlist.
shuffle :: Connection -> IO ()
shuffle = flip getResponse_ "shuffle"

-- | Retrieve metadata for songs in the current playlist.
playlistinfo :: Connection
            -> PLIndex   -- ^ Optional playlist index.
            -> IO [Song]
playlistinfo conn x = liftM takeSongs (getResponse conn cmd)
    where cmd = case x of
                    Pos x' -> "playlistinfo " ++ show (x' - 1)
                    ID x'  -> "playlistid " ++ show x'
                    _      -> "playlistinfo"

-- | Retrieve metadata for files in a given playlist.
listplaylistinfo :: Connection -> String -> IO [Song]
listplaylistinfo conn = liftM takeSongs . getResponse conn .
    ("listplaylistinfo " ++) . show

-- | Retrieve a list of files in a given playlist.
listplaylist :: Connection -> String -> IO [String]
listplaylist conn = liftM takeValues . getResponse conn .
    ("listplaylist " ++) . show

-- | Retrieve file paths and positions of songs in the current playlist.
-- Note that this command is only included for completeness sake; it's
-- deprecated and likely to disappear at any time.
playlist :: Connection -> IO [(PLIndex, String)]
playlist = liftM (map f) . flip getResponse "playlist"
    -- meh, the response here deviates from just about all other commands
    where f s = let (pos, name) = break (== ':') s
                in (Pos . (+1) $ read pos, drop 1 name)

-- | Retrieve a list of changed songs currently in the playlist since
-- a given playlist version.
plchanges :: Connection -> Integer -> IO [Song]
plchanges conn = liftM takeSongs . getResponse conn . ("plchanges " ++) . show

-- | Like 'plchanges' but only returns positions and ids.
plchangesposid :: Connection -> Integer -> IO [(PLIndex, PLIndex)]
plchangesposid conn plver =
    liftM (map takePosid . splitGroups . kvise) (getResponse conn cmd)
    where cmd          = "plchangesposid " ++ show plver
          takePosid xs = (Pos . (+1) $ takeNum "cpos" xs, ID $ takeNum "Id" xs)

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
crossfade conn = getResponse_ conn . ("crossfade " ++) . show

-- | Begin\/continue playing.
play :: Connection -> PLIndex -> IO ()
play conn PLNone  = getResponse_ conn "play"
play conn (Pos x) = getResponse_ conn ("play " ++ show (x-1))
play conn (ID x)  = getResponse_ conn ("playid " ++ show x)

-- | Pause playing.
pause :: Connection -> Bool -> IO ()
pause conn = getResponse_ conn . ("pause " ++) . showBool

-- | Stop playing.
stop :: Connection -> IO ()
stop = flip getResponse_ "stop"

-- | Play the next song.
next :: Connection -> IO ()
next = flip getResponse_ "next"

-- | Play the previous song.
previous :: Connection -> IO ()
previous = flip getResponse_ "previous"

-- | Seek to some point in a song.
-- Seeks in current song if no position is given.
seek :: Connection -> PLIndex -> Seconds -> IO ()
seek conn (Pos x) time =
    getResponse_ conn ("seek " ++ show (x - 1) ++ " " ++ show time)
seek conn (ID x) time =
    getResponse_ conn ("seekid " ++ show x ++ " " ++ show time)
seek conn PLNone time = do
    st <- status conn
    unless (stState st == Stopped) (seek conn (stSongID st) time)

-- | Set random playing.
random :: Connection -> Bool -> IO ()
random conn = getResponse_ conn . ("random " ++) . showBool

-- | Set repeating.
repeat :: Connection -> Bool -> IO ()
repeat conn = getResponse_ conn . ("repeat " ++) . showBool

-- | Set the volume.
setVolume :: Connection -> Int -> IO ()
setVolume conn = getResponse_ conn . ("setvol " ++) . show

-- | Increase or decrease volume by a given percent, e.g.
-- 'volume 10' will increase the volume by 10 percent, while
-- 'volume (-10)' will decrease it by the same amount.
-- Note that this command is only included for completeness sake ; it's
-- deprecated and may disappear at any time.
volume :: Connection -> Int -> IO ()
volume conn = getResponse_ conn . ("volume " ++) . show

--
-- Miscellaneous commands
--

-- | Clear the current error message in status.
clearerror :: Connection -> IO ()
clearerror (Conn h) = hPutStrLn h "clearerror" >> hClose h

-- | Close a MPD connection.
close :: Connection -> IO ()
close (Conn h) = hPutStrLn h "close" >> hClose h

-- | Retrieve a list of available commands.
commands :: Connection -> IO [String]
commands = liftM takeValues . flip getResponse "commands"

-- | Retrieve a list of unavailable commands.
notcommands :: Connection -> IO [String]
notcommands = liftM takeValues . flip getResponse "notcommands"

-- | Retrieve a list of available song metadata.
tagtypes :: Connection -> IO [String]
tagtypes = liftM takeValues . flip getResponse "tagtypes"

-- | Retrieve a list of supported urlhandlers.
urlhandlers :: Connection -> IO [String]
urlhandlers = liftM takeValues . flip getResponse "urlhandlers"

-- XXX should the password be quoted?
-- | Send password to server to authenticate session.
-- Password is sent as plain text.
password :: Connection -> String -> IO ()
password conn = getResponse_ conn . ("password " ++)

-- | Check that the server is still responding.
ping :: Connection -> IO ()
ping = flip getResponse_ "ping"

-- | Get server statistics.
stats :: Connection -> IO Stats
stats = liftM (parseStats . kvise) . flip getResponse "stats"
    where parseStats xs =
                Stats { stsArtists = takeNum "artists" xs,
                        stsAlbums = takeNum "albums" xs,
                        stsSongs = takeNum "songs" xs,
                        stsUptime = takeNum "uptime" xs,
                        stsPlaytime = takeNum "playtime" xs,
                        stsDbPlaytime = takeNum "db_playtime" xs,
                        stsDbUpdate = takeNum "db_update" xs }

-- | Get the server's status.
status :: Connection -> IO Status
status = liftM (parseStatus . kvise) . flip getResponse "status"
    where parseStatus xs =
              Status { stState = maybe Stopped parseState $ lookup "state" xs,
                     stVolume = takeNum "volume" xs,
                     stRepeat = takeBool "repeat" xs,
                     stRandom = takeBool "random" xs,
                     stPlaylistVersion = takeNum "playlist" xs,
                     stPlaylistLength = takeNum "playlistlength" xs,
                     stXFadeWidth = takeNum "xfade" xs,
                     stSongPos =
                         maybe PLNone (Pos . (1+) . read) $ lookup "song" xs,
                     stSongID = maybe PLNone (ID . read) $ lookup "songid" xs,
                     stTime = maybe (0,0) parseTime $ lookup "time" xs,
                     stBitrate = takeNum "bitrate" xs,
                     stAudio = maybe (0,0,0) parseAudio $ lookup "audio" xs,
                     stUpdatingDb = takeNum "updating_db" xs,
                     stError = takeString "error" xs
                   }
          parseState x = case x of "play"  -> Playing
                                   "pause" -> Paused
                                   _       -> Stopped
          parseTime  x = let (y,_:z) = break (== ':') x in (read y, read z)
          parseAudio x =
              let (u,_:u') = break (== ':') x; (v,_:w) = break (== ':') u' in
                  (read u, read v, read w)

--
-- Extensions\/shortcuts.
--

-- | Toggles play\/pause. Plays if stopped.
toggle :: Connection -> IO ()
toggle conn = do
    st <- status conn
    case stState st of
         Playing -> pause conn True
         _       -> play conn PLNone

-- | Add a list of songs\/folders to a playlist.
-- Should be more efficient than running 'add' many times.
addMany :: Connection -> Maybe String -> [String] -> IO ()
addMany _ _ [] = return ()
addMany conn plname [x] = add_ conn plname x
addMany conn plname xs = getResponses conn (map (cmd ++) xs) >> return ()
    where cmd = maybe ("add ") (\pl -> "playlistadd " ++ show pl ++ " ") plname

-- | Crop playlist.
crop :: Connection -> PLIndex -> PLIndex -> IO ()
crop _ (Pos _) (Pos _) = undefined
crop _ _ _ = return ()

-- | Search the database for songs relating to an artist.
findArtist :: Connection -> String -> IO [Song]
findArtist = flip find "artist"

-- | Search the database for songs relating to an album.
findAlbum :: Connection -> String -> IO [Song]
findAlbum = flip find "album"

-- | Search the database for songs relating to a song title.
findTitle :: Connection -> String -> IO [Song]
findTitle = flip find "title"

-- | List the artists in the database.
listArtists :: Connection -> IO [Artist]
listArtists = liftM takeValues . flip getResponse "list artist"

-- | List the albums in the database, optionally matching a given
-- artist.
listAlbums :: Connection -> Maybe Artist -> IO [Album]
listAlbums conn artist =
    liftM takeValues
          -- XXX according to the spec this shouldn't work (but it does)
          (getResponse conn ("list album " ++ maybe "" show artist))

-- | List the songs of an album of an artist.
listAlbum :: Connection -> Artist -> Album -> IO [Song]
listAlbum conn artist album = liftM (filter ((== artist) . sgArtist))
    (findAlbum conn album)

-- | Search the database for songs relating to an artist using 'search'.
searchArtist :: Connection -> String -> IO [Song]
searchArtist = flip search "artist"

-- | Search the database for songs relating to an album using 'search'.
searchAlbum :: Connection -> String -> IO [Song]
searchAlbum = flip search "album"

-- | Search the database for songs relating to a song title.
searchTitle :: Connection -> String -> IO [Song]
searchTitle = flip search "title"

-- | Retrieve the current playlist.
-- Equivalent to 'playlistinfo PLNone'.
getPlaylist :: Connection -> IO [Song]
getPlaylist = flip playlistinfo PLNone

--
-- Miscellaneous functions.
--

-- | Run getResponse but discard the response.
getResponse_ :: Connection -> String -> IO ()
getResponse_ c x = getResponse c x >> return ()

-- | Get the lines of the daemon's response to a given command.
getResponse :: Connection -> String -> IO [String]
getResponse (Conn h) cmd = hPutStrLn h cmd >> hFlush h >> f []
    where f acc = do
              l <- hGetLine h
              case l of
                  "OK"              -> return acc
                  ('A':'C':'K':_:e) -> fail e
                  _                 -> f (acc ++ [l])

-- | Get the lines of the daemon's response to a list of commands.
getResponses :: Connection -> [String] -> IO [String]
getResponses conn cmds = getResponse conn .
    unlines $ "command_list_begin" : cmds ++ ["command_list_end"]

-- | Break up a list of strings into an assoc list, separating at
-- the first ':'.
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
splitGroups :: Eq a => [(a, b)] -> [[(a, b)]]
splitGroups [] = []
splitGroups (x:xs) = ((x:us):splitGroups vs)
    where (us,vs) = break (\y -> fst x == fst y) xs

-- | Run 'kvise' and return only the values.
takeValues :: [String] -> [String]
takeValues = snd . unzip . kvise

-- | Build a list of song instances from a response.
-- Returns an empty list if input is empty.
takeSongs :: [String] -> [Song]
takeSongs = map takeSongInfo . splitGroups . kvise

-- |  Builds a song instance from an assoc list.
takeSongInfo :: [(String,String)] -> Song
takeSongInfo xs =
    Song {
          sgArtist    = takeString "Artist" xs,
          sgAlbum     = takeString "Album" xs,
          sgTitle     = takeString "Title" xs,
          sgGenre     = takeString "Genre" xs,
          sgName      = takeString "Name" xs,
          sgComposer  = takeString "Composer" xs,
          sgPerformer = takeString "Performer" xs,
          sgDate      = takeNum "Date" xs,
          sgTrack     = maybe (0, 0) parseTrack $ lookup "Track" xs,
          sgDisc      = maybe (0, 0) parseTrack $ lookup "Disc" xs,
          sgFilePath  = takeString "file" xs,
          sgLength    = takeNum "Time" xs,
          sgIndex     = maybe PLNone (ID . read) $ lookup "Id" xs
         }
    where parseTrack x = let (trck, tot) = break (== '/') x
                         in (read trck, parseNum (drop 1 tot))

-- Helpers for retrieving values from an assoc. list.
takeString :: String -> [(String, String)] -> String
takeString v = fromMaybe "" . lookup v

takeNum :: (Read a, Num a) => String -> [(String, String)] -> a
takeNum v = maybe 0 parseNum . lookup v

takeBool :: String -> [(String, String)] -> Bool
takeBool v = maybe False parseBool . lookup v

-- Parse a numeric value, returning 0 on failure.
parseNum :: (Read a, Num a) => String -> a
parseNum = fromMaybe 0 . maybeReads
    where maybeReads s = do ; [(x, "")] <- return (reads s) ; return x

-- Inverts 'parseBool'.
showBool :: Bool -> String
showBool x = if x then "1" else "0"

-- Parse a boolean response value.
parseBool :: String -> Bool
parseBool = (== "1") . take 1
