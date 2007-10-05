{-
    libmpd for Haskell, an MPD client library.
    Copyright (C) 2005-2007  Ben Sinclair <bsinclai@turing.une.edu.au>

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

-- | Module    : Network.MPD.Commands
-- Copyright   : (c) Ben Sinclair 2005-2007
-- License     : LGPL
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- Interface to the user commands supported by MPD.

module Network.MPD.Commands (
    -- * Command related data types
    State(..), Status(..), Stats(..),
    Device(..),
    Query(..), Meta(..),
    Artist, Album, Title, Seconds, PlaylistName, Path,
    PLIndex(..), Song(..), Count(..),

    -- * Admin commands
    disableOutput, enableOutput, kill, outputs, update,

    -- * Database commands
    find, list, listAll, listAllInfo, lsInfo, search, count,

    -- * Playlist commands
    -- $playlist
    add, add_, addId, clear, currentSong, delete, load, move,
    playlistInfo, listPlaylist, listPlaylistInfo, playlist, plChanges,
    plChangesPosId, playlistFind, playlistSearch, rm, rename, save, shuffle,
    swap,

    -- * Playback commands
    crossfade, next, pause, play, previous, random, repeat, seek, setVolume,
    volume, stop,

    -- * Miscellaneous commands
    clearError, close, commands, notCommands, password, ping, reconnect, stats,
    status, tagTypes, urlHandlers,

    -- * Extensions\/shortcuts
    addMany, deleteMany, crop, prune, lsDirs, lsFiles, lsPlaylists, findArtist,
    findAlbum, findTitle, listArtists, listAlbums, listAlbum, searchArtist,
    searchAlbum, searchTitle, getPlaylist, toggle, updateId
    ) where

import Network.MPD.Prim

import Control.Monad (liftM, unless)
import Prelude hiding (repeat)
import Data.List (findIndex, intersperse)
import Data.Maybe

--
-- Data types
--

type Artist       = String
type Album        = String
type Title        = String
type Seconds      = Integer
type PlaylistName = String
type Path         = String

-- | Available metadata types\/scope modifiers, used for searching the
-- database for entries with certain metadata values.
data Meta = Artist | Album | Title | Track | Name | Genre | Date
    | Composer | Performer | Disc | Any | Filename

instance Show Meta where
    show Artist    = "Artist"
    show Album     = "Album"
    show Title     = "Title"
    show Track     = "Track"
    show Name      = "Name"
    show Genre     = "Genre"
    show Date      = "Date"
    show Composer  = "Composer"
    show Performer = "Performer"
    show Disc      = "Disc"
    show Any       = "Any"
    show Filename  = "Filename"

-- | A query is composed of a scope modifier and a query string.
--
-- To match entries where album equals \"Foo\", use:
--
-- > Query Album "Foo"
--
-- To match entries where album equals \"Foo\" and artist equals \"Bar\", use:
--
-- > MultiQuery [Query Album "Foo", Query Artist "Bar"]
data Query = Query Meta String  -- ^ Simple query.
           | MultiQuery [Query] -- ^ Query with multiple conditions.

instance Show Query where
    show (Query meta query) = show meta ++ " " ++ show query
    show (MultiQuery xs)    = show xs
    showList xs _ = unwords $ map show xs

-- | Represents a song's playlist index.
data PLIndex = Pos Integer -- ^ A playlist position index (starting from 0)
             | ID Integer  -- ^ A playlist ID number that more robustly
                           --   identifies a song.
    deriving Show

-- | Represents the different playback states.
data State = Playing
           | Stopped
           | Paused
    deriving (Show, Eq)

-- | Container for MPD status.
data Status =
    Status { stState :: State
             -- | A percentage (0-100)
           , stVolume          :: Int
           , stRepeat          :: Bool
           , stRandom          :: Bool
             -- | A value that is incremented by the server every time the
             --   playlist changes.
           , stPlaylistVersion :: Integer
           , stPlaylistLength  :: Integer
             -- | Current song's position in the playlist.
           , stSongPos         :: Maybe PLIndex
             -- | Current song's playlist ID.
           , stSongID          :: Maybe PLIndex
             -- | Time elapsed\/total time.
           , stTime            :: (Seconds, Seconds)
             -- | Bitrate (in kilobytes per second) of playing song (if any).
           , stBitrate         :: Int
             -- | Crossfade time.
           , stXFadeWidth      :: Seconds
             -- | Samplerate\/bits\/channels for the chosen output device
             --   (see mpd.conf).
           , stAudio           :: (Int, Int, Int)
             -- | Job ID of currently running update (if any).
           , stUpdatingDb      :: Integer
             -- | Last error message (if any).
           , stError           :: String }
    deriving Show

-- | Container for database statistics.
data Stats =
    Stats { stsArtists    :: Integer -- ^ Number of artists.
          , stsAlbums     :: Integer -- ^ Number of albums.
          , stsSongs      :: Integer -- ^ Number of songs.
          , stsUptime     :: Seconds -- ^ Daemon uptime in seconds.
          , stsPlaytime   :: Seconds -- ^ Total playing time.
          , stsDbPlaytime :: Seconds -- ^ Total play time of all the songs in
                                     --   the database.
          , stsDbUpdate   :: Integer -- ^ Last database update in UNIX time.
          }
    deriving Show

-- | Represents a single song item.
data Song =
    Song { sgArtist, sgAlbum, sgTitle, sgFilePath, sgGenre, sgName, sgComposer
         , sgPerformer :: String
         , sgLength    :: Seconds       -- ^ Length in seconds
         , sgDate      :: Int           -- ^ Year
         , sgTrack     :: (Int, Int)    -- ^ Track number\/total tracks
         , sgDisc      :: (Int, Int)    -- ^ Position in set\/total in set
         , sgIndex     :: Maybe PLIndex }
    deriving Show

-- Avoid the need for writing a proper 'elem' for use in 'prune'.
instance Eq Song where
    (==) x y = sgFilePath x == sgFilePath y

-- | Represents the result of running 'count'.
data Count =
    Count { cSongs    :: Integer -- ^ Number of songs matching the query
          , cPlaytime :: Seconds -- ^ Total play time of matching songs
          }
    deriving Show

-- | Represents an output device.
data Device =
    Device { dOutputID      :: Int    -- ^ Output's ID number
           , dOutputName    :: String -- ^ Output's name as defined in the MPD
                                      --   configuration file
           , dOutputEnabled :: Bool }
    deriving Show

--
-- Admin commands
--

-- | Turn off an output device.
disableOutput :: (Conn c) => Int -> AbstractMPD c ()
disableOutput = getResponse_ . ("disableoutput " ++) . show

-- | Turn on an output device.
enableOutput :: (Conn c) => Int -> AbstractMPD c ()
enableOutput = getResponse_ . ("enableoutput " ++) . show

-- | Retrieve information for all output devices.
outputs :: (Conn c) => AbstractMPD c [Device]
outputs = liftM (map takeDevInfo . splitGroups . toAssoc)
    (getResponse "outputs")
    where
        takeDevInfo xs = Device {
            dOutputID      = takeNum "outputid" xs,
            dOutputName    = takeString "outputname" xs,
            dOutputEnabled = takeBool "outputenabled" xs
            }

-- | Update the server's database.
-- If no paths are given, all paths will be scanned.
update :: (Conn c) => [Path] -> AbstractMPD c ()
update  [] = getResponse_ "update"
update [x] = getResponse_ ("update " ++ show x)
update  xs = getResponses (map (("update " ++) . show) xs) >> return ()

--
-- Database commands
--

-- | List all metadata of metadata (sic).
list :: (Conn c)
     => Meta -- ^ Metadata to list
     -> Maybe Query -> AbstractMPD c [String]
list mtype query = liftM takeValues (getResponse cmd)
    where cmd = "list " ++ show mtype ++ maybe "" ((" "++) . show) query

-- | Non-recursively list the contents of a database directory.
lsInfo :: (Conn c) => Maybe Path -> AbstractMPD c [Either Path Song]
lsInfo path = do
    (dirs,_,songs) <- liftM takeEntries
                      (getResponse ("lsinfo " ++ maybe "" show path))
    return (map Left dirs ++ map Right songs)

-- | List the songs (without metadata) in a database directory recursively.
listAll :: (Conn c) => Maybe Path -> AbstractMPD c [Path]
listAll path = liftM (map snd . filter ((== "file") . fst) . toAssoc)
                     (getResponse ("listall " ++ maybe "" show path))

-- | Recursive 'lsInfo'.
listAllInfo :: (Conn c) => Maybe Path -> AbstractMPD c [Either Path Song]
listAllInfo path = do
    (dirs,_,songs) <- liftM takeEntries
                      (getResponse ("listallinfo " ++ maybe "" show path))
    return (map Left dirs ++ map Right songs)

-- | Search the database for entries exactly matching a query.
find :: (Conn c) => Query -> AbstractMPD c [Song]
find query = liftM takeSongs (getResponse ("find " ++ show query))

-- | Search the database using case insensitive matching.
search :: (Conn c) => Query -> AbstractMPD c [Song]
search query = liftM takeSongs (getResponse ("search " ++ show query))

-- | Count the number of entries matching a query.
count :: (Conn c) => Query -> AbstractMPD c Count
count query = liftM (takeCountInfo . toAssoc)
                    (getResponse ("count " ++ show query))
    where takeCountInfo xs = Count { cSongs    = takeNum "songs" xs,
                                     cPlaytime = takeNum "playtime" xs }

--
-- Playlist commands
--
-- $playlist
-- Unless otherwise noted all playlist commands operate on the current
-- playlist.

-- | Like 'add', but returns a playlist id.
addId :: (Conn c) => Path -> AbstractMPD c Integer
addId x =
    liftM (read . snd . head . toAssoc) (getResponse ("addid " ++ show x))

-- | Like 'add_' but returns a list of the files added.
add :: (Conn c) => Maybe PlaylistName -> Path -> AbstractMPD c [Path]
add plname x = add_ plname x >> listAll (Just x)

-- | Add a song (or a whole directory) to a playlist.
-- Adds to current if no playlist is specified.
-- Will create a new playlist if the one specified does not already exist.
add_ :: (Conn c) => Maybe PlaylistName -> Path -> AbstractMPD c ()
add_ Nothing       = getResponse_ . ("add " ++) . show
add_ (Just plname) = getResponse_ .
                     (("playlistadd " ++ show plname ++ " ") ++) . show

-- | Clear a playlist. Clears current playlist if no playlist is specified.
-- If the specified playlist does not exist, it will be created.
clear :: (Conn c) => Maybe PlaylistName -> AbstractMPD c ()
clear = getResponse_ . maybe "clear" (("playlistclear " ++) . show)

-- | Remove a song from a playlist.
-- If no playlist is specified, current playlist is used.
-- Note that a playlist position ('Pos') is required when operating on
-- playlists other than the current.
delete :: (Conn c) => Maybe PlaylistName -> PLIndex -> AbstractMPD c ()
delete Nothing (Pos x) = getResponse_ ("delete " ++ show x)
delete Nothing (ID x) = getResponse_ ("deleteid " ++ show x)
delete (Just plname) (Pos x) =
    getResponse_ ("playlistdelete " ++ show plname ++ " " ++ show x)
delete _ _ = fail "'delete' within a playlist doesn't accept a playlist ID"

-- | Load an existing playlist.
load :: (Conn c) => PlaylistName -> AbstractMPD c ()
load = getResponse_ . ("load " ++) . show

-- | Move a song to a given position.
-- Note that a playlist position ('Pos') is required when operating on
-- playlists other than the current.
move :: (Conn c) => Maybe PlaylistName -> PLIndex -> Integer -> AbstractMPD c ()
move Nothing (Pos from) to =
    getResponse_ ("move " ++ show from ++ " " ++ show to)
move Nothing (ID from) to =
    getResponse_ ("moveid " ++ show from ++ " " ++ show to)
move (Just plname) (Pos from) to =
    getResponse_ ("playlistmove " ++ show plname ++ " " ++ show from ++
                       " " ++ show to)
move _ _ _ = fail "'move' within a playlist doesn't accept a playlist ID"

-- | Delete existing playlist.
rm :: (Conn c) => PlaylistName -> AbstractMPD c ()
rm = getResponse_ . ("rm " ++) . show

-- | Rename an existing playlist.
rename :: (Conn c)
       => PlaylistName -- ^ Original playlist
       -> PlaylistName -- ^ New playlist name
       -> AbstractMPD c ()
rename plname new =
    getResponse_ ("rename " ++ show plname ++ " " ++ show new)

-- | Save the current playlist.
save :: (Conn c) => PlaylistName -> AbstractMPD c ()
save = getResponse_ . ("save " ++) . show

-- | Swap the positions of two songs.
-- Note that the positions must be of the same type, i.e. mixing 'Pos' and 'ID'
-- will result in a no-op.
swap :: (Conn c) => PLIndex -> PLIndex -> AbstractMPD c ()
swap (Pos x) (Pos y) = getResponse_ ("swap "   ++ show x ++ " " ++ show y)
swap (ID x)  (ID y)  = getResponse_ ("swapid " ++ show x ++ " " ++ show y)
swap _ _ = fail "'swap' cannot mix position and ID arguments"

-- | Shuffle the playlist.
shuffle :: (Conn c) => AbstractMPD c ()
shuffle = getResponse_ "shuffle"

-- | Retrieve metadata for songs in the current playlist.
playlistInfo :: (Conn c) => Maybe PLIndex -> AbstractMPD c [Song]
playlistInfo x = liftM takeSongs (getResponse cmd)
    where cmd = case x of
                    Just (Pos x') -> "playlistinfo " ++ show x'
                    Just (ID x')  -> "playlistid " ++ show x'
                    Nothing       -> "playlistinfo"

-- | Retrieve metadata for files in a given playlist.
listPlaylistInfo :: (Conn c) => PlaylistName -> AbstractMPD c [Song]
listPlaylistInfo = liftM takeSongs . getResponse .
    ("listplaylistinfo " ++) . show

-- | Retrieve a list of files in a given playlist.
listPlaylist :: (Conn c) => PlaylistName -> AbstractMPD c [Path]
listPlaylist = liftM takeValues . getResponse . ("listplaylist " ++) . show

-- | Retrieve file paths and positions of songs in the current playlist.
-- Note that this command is only included for completeness sake; it's
-- deprecated and likely to disappear at any time, please use 'playlistInfo'
-- instead.
playlist :: (Conn c) => AbstractMPD c [(PLIndex, Path)]
playlist = liftM (map f) (getResponse "playlist")
    where f s = let (pos, name) = break (== ':') s
                in (Pos $ read pos, drop 1 name)

-- | Retrieve a list of changed songs currently in the playlist since
-- a given playlist version.
plChanges :: (Conn c) => Integer -> AbstractMPD c [Song]
plChanges = liftM takeSongs . getResponse . ("plchanges " ++) . show

-- | Like 'plChanges' but only returns positions and ids.
plChangesPosId :: (Conn c) => Integer -> AbstractMPD c [(PLIndex, PLIndex)]
plChangesPosId plver =
    liftM (map takePosid . splitGroups . toAssoc) (getResponse cmd)
    where cmd          = "plchangesposid " ++ show plver
          takePosid xs = (Pos $ takeNum "cpos" xs, ID $ takeNum "Id" xs)

-- | Search for songs in the current playlist with strict matching.
playlistFind :: (Conn c) => Query -> AbstractMPD c [Song]
playlistFind = liftM takeSongs . getResponse . ("playlistfind " ++) . show

-- | Search case-insensitively with partial matches for songs in the
-- current playlist.
playlistSearch :: (Conn c) => Query -> AbstractMPD c [Song]
playlistSearch = liftM takeSongs . getResponse . ("playlistsearch " ++) . show

-- | Get the currently playing song.
currentSong :: (Conn c) => AbstractMPD c (Maybe Song)
currentSong = do
    currStatus <- status
    if stState currStatus == Stopped
        then return Nothing
        else do ls <- liftM toAssoc (getResponse "currentsong")
                return $ if null ls then Nothing
                                    else Just (takeSongInfo ls)

--
-- Playback commands
--

-- | Set crossfading between songs.
crossfade :: (Conn c) => Seconds -> AbstractMPD c ()
crossfade = getResponse_ . ("crossfade " ++) . show

-- | Begin\/continue playing.
play :: (Conn c) => Maybe PLIndex -> AbstractMPD c ()
play Nothing        = getResponse_ "play"
play (Just (Pos x)) = getResponse_ ("play " ++ show x)
play (Just (ID x))  = getResponse_ ("playid " ++ show x)

-- | Pause playing.
pause :: (Conn c) => Bool -> AbstractMPD c ()
pause = getResponse_ . ("pause " ++) . showBool

-- | Stop playing.
stop :: (Conn c) => AbstractMPD c ()
stop = getResponse_ "stop"

-- | Play the next song.
next :: (Conn c) => AbstractMPD c ()
next = getResponse_ "next"

-- | Play the previous song.
previous :: (Conn c) => AbstractMPD c ()
previous = getResponse_ "previous"

-- | Seek to some point in a song.
-- Seeks in current song if no position is given.
seek :: (Conn c) => Maybe PLIndex -> Seconds -> AbstractMPD c ()
seek (Just (Pos x)) time =
    getResponse_ ("seek " ++ show x ++ " " ++ show time)
seek (Just (ID x)) time =
    getResponse_ ("seekid " ++ show x ++ " " ++ show time)
seek Nothing time = do
    st <- status
    unless (stState st == Stopped) (seek (stSongID st) time)

-- | Set random playing.
random :: (Conn c) => Bool -> AbstractMPD c ()
random = getResponse_ . ("random " ++) . showBool

-- | Set repeating.
repeat :: (Conn c) => Bool -> AbstractMPD c ()
repeat = getResponse_ . ("repeat " ++) . showBool

-- | Set the volume (0-100 percent).
setVolume :: (Conn c) => Int -> AbstractMPD c ()
setVolume = getResponse_ . ("setvol " ++) . show

-- | Increase or decrease volume by a given percent, e.g.
-- 'volume 10' will increase the volume by 10 percent, while
-- 'volume (-10)' will decrease it by the same amount.
-- Note that this command is only included for completeness sake ; it's
-- deprecated and may disappear at any time, please use 'setVolume' instead.
volume :: (Conn c) => Int -> AbstractMPD c ()
volume = getResponse_ . ("volume " ++) . show

--
-- Miscellaneous commands
--

-- | Clear the current error message in status.
clearError :: (Conn c) => AbstractMPD c ()
clearError = getResponse_ "clearerror"

-- | Retrieve a list of available commands.
commands :: (Conn c) => AbstractMPD c [String]
commands = liftM takeValues (getResponse "commands")

-- | Retrieve a list of unavailable commands.
notCommands :: (Conn c) => AbstractMPD c [String]
notCommands = liftM takeValues (getResponse "notcommands")

-- | Retrieve a list of available song metadata.
tagTypes :: (Conn c) => AbstractMPD c [String]
tagTypes = liftM takeValues (getResponse "tagtypes")

-- | Retrieve a list of supported urlhandlers.
urlHandlers :: (Conn c) => AbstractMPD c [String]
urlHandlers = liftM takeValues (getResponse "urlhandlers")

-- XXX should the password be quoted?
-- | Send password to server to authenticate session.
-- Password is sent as plain text.
password :: (Conn c) => String -> AbstractMPD c ()
password = getResponse_ . ("password " ++)

-- | Check that the server is still responding.
ping :: (Conn c) => AbstractMPD c ()
ping = getResponse_ "ping"

-- | Get server statistics.
stats :: (Conn c) => AbstractMPD c Stats
stats = liftM (parseStats . toAssoc) (getResponse "stats")
    where parseStats xs =
                Stats { stsArtists = takeNum "artists" xs,
                        stsAlbums = takeNum "albums" xs,
                        stsSongs = takeNum "songs" xs,
                        stsUptime = takeNum "uptime" xs,
                        stsPlaytime = takeNum "playtime" xs,
                        stsDbPlaytime = takeNum "db_playtime" xs,
                        stsDbUpdate = takeNum "db_update" xs }

-- | Get the server's status.
status :: (Conn c) => AbstractMPD c Status
status = liftM (parseStatus . toAssoc) (getResponse "status")
    where parseStatus xs =
              Status { stState = maybe Stopped parseState $ lookup "state" xs,
                       stVolume = takeNum "volume" xs,
                       stRepeat = takeBool "repeat" xs,
                       stRandom = takeBool "random" xs,
                       stPlaylistVersion = takeNum "playlist" xs,
                       stPlaylistLength = takeNum "playlistlength" xs,
                       stXFadeWidth = takeNum "xfade" xs,
                       stSongPos = takeIndex Pos "song" xs,
                       stSongID = takeIndex ID "songid" xs,
                       stTime = maybe (0,0) parseTime $ lookup "time" xs,
                       stBitrate = takeNum "bitrate" xs,
                       stAudio = maybe (0,0,0) parseAudio $ lookup "audio" xs,
                       stUpdatingDb = takeNum "updating_db" xs,
                       stError = takeString "error" xs }
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

-- | Like 'update', but returns the update job id.
updateId :: (Conn c) => [Path] -> AbstractMPD c Integer
updateId paths = liftM (read . head . takeValues) cmd
  where cmd = case paths of
                []  -> getResponse "update"
                [x] -> getResponse ("update " ++ x)
                xs  -> getResponses (map ("update " ++) xs)

-- | Toggles play\/pause. Plays if stopped.
toggle :: (Conn c) => AbstractMPD c ()
toggle = status >>= \st -> case stState st of Playing -> pause True
                                              _       -> play Nothing

-- | Add a list of songs\/folders to a playlist.
-- Should be more efficient than running 'add' many times.
addMany :: (Conn c) => Maybe PlaylistName -> [Path] -> AbstractMPD c ()
addMany _ [] = return ()
addMany plname [x] = add_ plname x
addMany plname xs = getResponses (map (cmd ++) xs) >> return ()
    where cmd = maybe ("add ") (\pl -> "playlistadd " ++ show pl ++ " ") plname

-- | Delete a list of songs from a playlist.
-- If there is a duplicate then no further songs will be deleted, so
-- take care to avoid them (see 'prune' for this).
deleteMany :: (Conn c) => Maybe PlaylistName -> [PLIndex] -> AbstractMPD c ()
deleteMany _ [] = return ()
deleteMany plname [x] = delete plname x
deleteMany (Just plname) xs = getResponses (map cmd xs) >> return ()
    where cmd (Pos x) = "playlistdelete " ++ show plname ++ " " ++ show x
          cmd _       = ""
deleteMany Nothing xs = getResponses (map cmd xs) >> return ()
    where cmd (Pos x) = "delete " ++ show x
          cmd (ID x)  = "deleteid " ++ show x

-- | Crop playlist.
-- The bounds are inclusive.
-- If 'Nothing' or 'ID' is passed the cropping will leave your playlist alone
-- on that side.
crop :: (Conn c) => Maybe PLIndex -> Maybe PLIndex -> AbstractMPD c ()
crop x y = do
    pl <- playlistInfo Nothing
    let x' = case x of Just (Pos p) -> fromInteger p
                       Just (ID i)  -> maybe 0 id (findByID i pl)
                       Nothing      -> 0
        -- ensure that no songs are deleted twice with 'max'.
        ys = case y of Just (Pos p) -> drop (max (fromInteger p) x') pl
                       Just (ID i)  -> maybe [] (flip drop pl . max x' . (+1))
                                      (findByID i pl)
                       Nothing      -> []
    deleteMany Nothing . mapMaybe sgIndex $ take x' pl ++ ys
    where findByID i = findIndex ((==) i . (\(ID j) -> j) . fromJust . sgIndex)

-- | Remove duplicate playlist entries.
prune :: (Conn c) => AbstractMPD c ()
prune = findDuplicates >>= deleteMany Nothing

-- Find duplicate playlist entries.
findDuplicates :: (Conn c) => AbstractMPD c [PLIndex]
findDuplicates =
    liftM (map ((\(ID x) -> ID x) . fromJust . sgIndex) . flip dups ([],[])) $
        playlistInfo Nothing
    where dups [] (_, dup) = dup
          dups (x:xs) (ys, dup)
            | x `elem` xs && x `notElem` ys = dups xs (ys, x:dup)
            | otherwise                     = dups xs (x:ys, dup)

-- | List directories non-recursively.
lsDirs :: (Conn c) => Maybe Path -> AbstractMPD c [Path]
lsDirs path = liftM ((\(x,_,_) -> x) . takeEntries)
                    (getResponse ("lsinfo " ++ maybe "" show path))

-- | List files non-recursively.
lsFiles :: (Conn c) => Maybe Path -> AbstractMPD c [Path]
lsFiles path = liftM (map sgFilePath . (\(_,_,x) -> x) . takeEntries)
                     (getResponse ("lsinfo " ++ maybe "" show path))

-- | List all playlists.
lsPlaylists :: (Conn c) => AbstractMPD c [PlaylistName]
lsPlaylists = liftM ((\(_,x,_) -> x) . takeEntries) (getResponse "lsinfo")

-- | Search the database for songs relating to an artist.
findArtist :: (Conn c) => Artist -> AbstractMPD c [Song]
findArtist = find . Query Artist

-- | Search the database for songs relating to an album.
findAlbum :: (Conn c) => Album -> AbstractMPD c [Song]
findAlbum = find . Query Album

-- | Search the database for songs relating to a song title.
findTitle :: (Conn c) => Title -> AbstractMPD c [Song]
findTitle = find . Query Title

-- | List the artists in the database.
listArtists :: (Conn c) => AbstractMPD c [Artist]
listArtists = liftM takeValues (getResponse "list artist")

-- | List the albums in the database, optionally matching a given
-- artist.
listAlbums :: (Conn c) => Maybe Artist -> AbstractMPD c [Album]
listAlbums artist = liftM takeValues (getResponse ("list album" ++
    maybe "" ((" artist " ++) . show) artist))

-- | List the songs in an album of some artist.
listAlbum :: (Conn c) => Artist -> Album -> AbstractMPD c [Song]
listAlbum artist album = find (MultiQuery [Query Artist artist
                                          ,Query Album album])

-- | Search the database for songs relating to an artist using 'search'.
searchArtist :: (Conn c) => Artist -> AbstractMPD c [Song]
searchArtist = search . Query Artist

-- | Search the database for songs relating to an album using 'search'.
searchAlbum :: (Conn c) => Album -> AbstractMPD c [Song]
searchAlbum = search . Query Album

-- | Search the database for songs relating to a song title.
searchTitle :: (Conn c) => Title -> AbstractMPD c [Song]
searchTitle = search . Query Title

-- | Retrieve the current playlist.
-- Equivalent to 'playlistInfo Nothing'.
getPlaylist :: (Conn c) => AbstractMPD c [Song]
getPlaylist = playlistInfo Nothing

--
-- Miscellaneous functions.
--

-- Run getResponse but discard the response.
getResponse_ :: (Conn c) => String -> AbstractMPD c ()
getResponse_ x = getResponse x >> return ()

-- Get the lines of the daemon's response to a list of commands.
getResponses :: (Conn c) => [String] -> AbstractMPD c [String]
getResponses cmds = getResponse (concat . intersperse "\n" $ cmds')
    where cmds' = "command_list_begin" : cmds ++ ["command_list_end"]

--
-- Parsing.
--

-- Run 'toAssoc' and return only the values.
takeValues :: [String] -> [String]
takeValues = snd . unzip . toAssoc

-- Separate the result of an lsinfo\/listallinfo call into directories,
-- playlists, and songs.
takeEntries :: [String] -> ([String], [String], [Song])
takeEntries s =
    (dirs, playlists, map takeSongInfo . splitGroups $ reverse filedata)
    where (dirs, playlists, filedata) = foldl split ([], [], []) $ toAssoc s
          split (ds, pls, ss) x@(k, v) | k == "directory" = (v:ds, pls, ss)
                                       | k == "playlist"  = (ds, v:pls, ss)
                                       | otherwise        = (ds, pls, x:ss)

-- Build a list of song instances from a response.
takeSongs :: [String] -> [Song]
takeSongs = map takeSongInfo . splitGroups . toAssoc

-- Builds a song instance from an assoc. list.
takeSongInfo :: [(String,String)] -> Song
takeSongInfo xs =
    Song { sgArtist    = takeString "Artist" xs,
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
           sgIndex     = takeIndex ID "Id" xs }
    where parseTrack x = let (trck, tot) = break (== '/') x
                         in (read trck, parseNum (drop 1 tot))

-- Helpers for retrieving values from an assoc. list.
takeString :: String -> [(String, String)] -> String
takeString v = fromMaybe "" . lookup v

takeIndex :: (Integer -> PLIndex) -> String -> [(String, String)]
          -> Maybe PLIndex
takeIndex c v = maybe Nothing (Just . c . parseNum) . lookup v

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

-- Break up a list of strings into an assoc. list, separating at
-- the first ':'.
toAssoc :: [String] -> [(String, String)]
toAssoc = map f
    where f x = let (k,v) = break (== ':') x in
                (k,dropWhile (== ' ') $ drop 1 v)

-- Takes an assoc. list with recurring keys, and groups each cycle of
-- keys with their values together. The first key of each cycle needs
-- to be present in every cycle for it to work, but the rest don't
-- affect anything.
--
-- > splitGroups [(1,'a'),(2,'b'),(1,'c'),(2,'d')] ==
-- >     [[(1,'a'),(2,'b')],[(1,'c'),(2,'d')]]
splitGroups :: Eq a => [(a, b)] -> [[(a, b)]]
splitGroups [] = []
splitGroups (x:xs) = ((x:us):splitGroups vs)
    where (us,vs) = break (\y -> fst x == fst y) xs
