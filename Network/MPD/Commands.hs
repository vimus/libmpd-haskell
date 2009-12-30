{-# LANGUAGE PatternGuards #-}

-- | Module    : Network.MPD.Commands
-- Copyright   : (c) Ben Sinclair 2005-2009
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
--
-- Interface to the user commands supported by MPD.

module Network.MPD.Commands (
    -- * Command related data types
    module Network.MPD.Commands.Types,

    -- * Query interface
    module Network.MPD.Commands.Query,

    -- * Admin commands
    disableOutput, enableOutput, kill, outputs, update, rescan,
    idle, noidle,

    -- * Database commands
    find, list, listAll, listAllInfo, lsInfo, search, count,

    -- * Playlist commands
    -- $playlist
    add, add_, addId, findAdd, playlistAdd_, playlistAdd, clear,
    playlistClear, currentSong, delete, playlistDelete, load, move,
    playlistMove, playlistInfo, listPlaylist, listPlaylists, listPlaylistInfo,
    playlist, plChanges, plChangesPosId, playlistFind, playlistSearch, rm,
    rename, save, shuffle, swap,

    -- * Playback commands
    crossfade, next, pause, play, previous, random, repeat, single, consume,
    seek, setVolume, stop, replayGainMode, replayGainStatus,

    -- * Miscellaneous commands
    clearError, commands, notCommands, password, ping, stats, status,
    tagTypes, urlHandlers, decoders,

    -- * Extensions\/shortcuts
    addMany, deleteMany, complete, crop, prune, lsDirs, lsFiles, lsPlaylists,
    listArtists, listAlbums, listAlbum, getPlaylist, toggle, updateId, volume
    ) where

import Network.MPD.Commands.Arg
import Network.MPD.Commands.Parse
import Network.MPD.Commands.Query
import Network.MPD.Commands.Types
import Network.MPD.Core
import Network.MPD.Utils

import Control.Monad (liftM, unless)
import Control.Monad.Error (throwError)
import Prelude hiding (repeat)
import Data.List (findIndex, intersperse, isPrefixOf)
import Data.Maybe
import System.FilePath (dropFileName)

--
-- Admin commands
--

-- | Turn off an output device.
disableOutput :: MonadMPD m => Int -> m ()
disableOutput = getResponse_ . ("disableoutput" <$>)

-- | Turn on an output device.
enableOutput :: MonadMPD m => Int -> m ()
enableOutput = getResponse_ . ("enableoutput" <$>)

-- | Retrieve information for all output devices.
outputs :: MonadMPD m => m [Device]
outputs = getResponse "outputs" >>= runParser parseOutputs

-- | Update the server's database.
-- If no paths are given, all paths will be scanned.
-- Unreadable or non-existent paths are silently ignored.
update :: MonadMPD m => [Path] -> m ()
update  [] = getResponse_ "update"
update [x] = getResponse_ ("update" <$> x)
update xs  = getResponses (map ("update" <$>) xs) >> return ()

-- | Like 'update' but also rescans unmodified files.
rescan :: MonadMPD m => [Path] -> m ()
rescan []  = getResponse_ "rescan"
rescan [x] = getResponse_ ("rescan" <$> x)
rescan xs  = getResponses (map ("rescan" <$>) xs) >> return ()

--
-- Database commands
--

-- | Wait until there is a noteworthy change in one or more of MPD's
-- susbystems. Note that running this command will block until either 'idle'
-- returns or is cancelled by 'noidle'.
idle :: MonadMPD m => m [Subsystem]
idle = do
    mapM (\("changed", system) -> case system of "database" -> return Database
                                                 "update"   -> return Update
                                                 "stored_playlist" -> return StoredPlaylist
                                                 "playlist" -> return Playlist
                                                 "player" -> return Player
                                                 "mixer" -> return Mixer
                                                 "output" -> return Output
                                                 "options" -> return Options
                                                 k -> fail ("Unknown subsystem: " ++ k))
         =<< toAssocList `liftM` getResponse "idle"

-- | Cancel 'idle'.
noidle :: MonadMPD m => m ()
noidle = getResponse_ "noidle"

-- | List all metadata of metadata (sic).
list :: MonadMPD m
     => Meta -- ^ Metadata to list
     -> Query -> m [String]
list mtype query = liftM takeValues $ getResponse ("list" <$> mtype <++> query)

-- | Non-recursively list the contents of a database directory.
lsInfo :: MonadMPD m => Path -> m [Either Path Song]
lsInfo = lsInfo' "lsinfo"

-- | List the songs (without metadata) in a database directory recursively.
listAll :: MonadMPD m => Path -> m [Path]
listAll path = liftM (map snd . filter ((== "file") . fst) . toAssocList)
                     (getResponse $ "listall" <$> path)

-- | Recursive 'lsInfo'.
listAllInfo :: MonadMPD m => Path -> m [Either Path Song]
listAllInfo = lsInfo' "listallinfo"

-- Helper for lsInfo and listAllInfo.
lsInfo' :: MonadMPD m => String -> Path -> m [Either Path Song]
lsInfo' cmd path =
    liftM (extractEntries (Just . Right, const Nothing, Just . Left)) $
         takeEntries =<< getResponse (cmd <$> path)

-- | Search the database for entries exactly matching a query.
find :: MonadMPD m => Query -> m [Song]
find query = getResponse ("find" <$> query) >>= takeSongs

-- | Search the database using case insensitive matching.
search :: MonadMPD m => Query -> m [Song]
search query = getResponse ("search" <$> query) >>= takeSongs

-- | Count the number of entries matching a query.
count :: MonadMPD m => Query -> m Count
count query = getResponse ("count" <$>  query) >>= runParser parseCount

--
-- Playlist commands
--
-- $playlist
-- Unless otherwise noted all playlist commands operate on the current
-- playlist.

-- This might do better to throw an exception than silently return 0.
-- | Like 'add', but returns a playlist id.
addId :: MonadMPD m => Path -> Maybe Integer -- ^ Optional playlist position
      -> m Integer
addId p pos = liftM (parse parseNum id 0 . snd . head . toAssocList)
              $ getResponse1 ("addid" <$> p <++> pos)

-- | Like 'add_' but returns a list of the files added.
add :: MonadMPD m => Path -> m [Path]
add x = add_ x >> listAll x

-- | Add a song (or a whole directory) to the current playlist.
add_ :: MonadMPD m => Path -> m ()
add_ path = getResponse_ ("add" <$> path)

-- | Like 'playlistAdd' but returns a list of the files added.
playlistAdd :: MonadMPD m => PlaylistName -> Path -> m [Path]
playlistAdd plname path = playlistAdd_ plname path >> listAll path

-- | Add a song (or a whole directory) to a stored playlist.
-- Will create a new playlist if the one specified does not already exist.
playlistAdd_ :: MonadMPD m => PlaylistName -> Path -> m ()
playlistAdd_ plname path = getResponse_ ("playlistadd" <$> plname <++> path)

-- | Adds songs matching a query to the current playlist.
findAdd :: MonadMPD m => Query -> m ()
findAdd q = getResponse_ ("findadd" <$> q)

-- | Clear the current playlist.
clear :: MonadMPD m => m ()
clear = getResponse_ "clear"

-- | Clear a playlist. If the specified playlist does not exist, it will be
-- created.
playlistClear :: MonadMPD m => PlaylistName -> m ()
playlistClear = getResponse_ . ("playlistclear" <$>)

-- | Remove a song from the current playlist.
delete :: MonadMPD m => PLIndex -> m ()
delete (Pos x) = getResponse_ ("delete" <$> x)
delete (ID x)  = getResponse_ ("deleteid" <$> x)

-- | Remove a song from a playlist.
playlistDelete :: MonadMPD m => PlaylistName
               -> Integer -- ^ Playlist position
               -> m ()
playlistDelete name pos = getResponse_ ("playlistdelete" <$> name <++> pos)

-- | Load an existing playlist.
load :: MonadMPD m => PlaylistName -> m ()
load plname = getResponse_ ("load" <$> plname)

-- | Move a song to a given position in the current playlist.
move :: MonadMPD m => PLIndex -> Integer -> m ()
move (Pos from) to = getResponse_ ("move" <$> from <++> to)
move (ID from) to = getResponse_ ("moveid" <$> from <++> to)

-- | Move a song to a given position in the playlist specified.
playlistMove :: MonadMPD m => PlaylistName -> Integer -> Integer -> m ()
playlistMove name from to =
    getResponse_ ("playlistmove" <$> name <++> from <++> to)

-- | Delete existing playlist.
rm :: MonadMPD m => PlaylistName -> m ()
rm plname = getResponse_ ("rm" <$> plname)

-- | Rename an existing playlist.
rename :: MonadMPD m
       => PlaylistName -- ^ Original playlist
       -> PlaylistName -- ^ New playlist name
       -> m ()
rename plname new = getResponse_ ("rename" <$> plname <++> new)

-- | Save the current playlist.
save :: MonadMPD m => PlaylistName -> m ()
save plname = getResponse_ ("save" <$> plname)

-- | Swap the positions of two songs.
-- Note that the positions must be of the same type, i.e. mixing 'Pos' and 'ID'
-- will result in a no-op.
swap :: MonadMPD m => PLIndex -> PLIndex -> m ()
swap (Pos x) (Pos y) = getResponse_ ("swap" <$> x <++> y)
swap (ID x)  (ID y)  = getResponse_ ("swapid" <$> x <++> y)
swap _ _ = fail "'swap' cannot mix position and ID arguments"

-- | Shuffle the playlist.
shuffle :: MonadMPD m => Maybe (Int, Int) -- ^ Optional range (start, end)
        -> m ()
shuffle range = getResponse_ ("shuffle" <$> range)

-- | Retrieve metadata for songs in the current playlist.
playlistInfo :: MonadMPD m => Maybe (Either PLIndex (Int, Int)) -> m [Song]
playlistInfo x = getResponse cmd >>= takeSongs
    where cmd = case x of
                    Nothing -> "playlistinfo"
                    Just (Left (Pos x')) -> "playlistinfo" <$> x'
                    Just (Left (ID x'))  -> "playlistid" <$> x'
                    Just (Right range)   -> "playlistinfo" <$> range

-- | Retrieve metadata for files in a given playlist.
listPlaylistInfo :: MonadMPD m => PlaylistName -> m [Song]
listPlaylistInfo plname =
    takeSongs =<< getResponse ("listplaylistinfo" <$> plname)

-- | Retrieve a list of files in a given playlist.
listPlaylist :: MonadMPD m => PlaylistName -> m [Path]
listPlaylist plname =
    liftM takeValues $ getResponse ("listplaylist" <$> plname)

-- | Retreive a list of stored playlists.
listPlaylists :: MonadMPD m => m [PlaylistName]
listPlaylists = (go [] . toAssocList) `liftM` getResponse "listplaylists"
    where
        -- After each playlist name we get a timestamp
        go acc [] = acc
        go acc ((_, b):_:xs) = go (b : acc) xs
        go _ _ = error "listPlaylists: bug"

-- | Retrieve file paths and positions of songs in the current playlist.
-- Note that this command is only included for completeness sake; it's
-- deprecated and likely to disappear at any time, please use 'playlistInfo'
-- instead.
playlist :: MonadMPD m => m [(PLIndex, Path)]
playlist = mapM f =<< getResponse "playlist"
    where f s | (pos, name) <- breakChar ':' s
              , Just pos'   <- parseNum pos
              = return (Pos pos', name)
              | otherwise = throwError . Unexpected $ show s

-- | Retrieve a list of changed songs currently in the playlist since
-- a given playlist version.
plChanges :: MonadMPD m => Integer -> m [Song]
plChanges version = takeSongs =<< getResponse ("plchanges" <$> version)

-- | Like 'plChanges' but only returns positions and ids.
plChangesPosId :: MonadMPD m => Integer -> m [(PLIndex, PLIndex)]
plChangesPosId plver =
    getResponse ("plchangesposid" <$> plver) >>=
    mapM f . splitGroups [("cpos",id)] . toAssocList
    where f xs | [("cpos", x), ("Id", y)] <- xs
               , Just (x', y') <- pair parseNum (x, y)
               = return (Pos x', ID y')
               | otherwise = throwError . Unexpected $ show xs

-- | Search for songs in the current playlist with strict matching.
playlistFind :: MonadMPD m => Query -> m [Song]
playlistFind q = takeSongs =<< getResponse ("playlistfind" <$> q)

-- | Search case-insensitively with partial matches for songs in the
-- current playlist.
playlistSearch :: MonadMPD m => Query -> m [Song]
playlistSearch q = takeSongs =<< getResponse ("playlistsearch" <$> q)

-- | Get the currently playing song.
currentSong :: (Functor m, MonadMPD m) => m (Maybe Song)
currentSong = do
    cs <- status
    if stState cs == Stopped
       then return Nothing
       else getResponse1 "currentsong" >>=
            fmap Just . runParser parseSong . toAssocList

--
-- Playback commands
--

-- | Set crossfading between songs.
crossfade :: MonadMPD m => Seconds -> m ()
crossfade secs = getResponse_ ("crossfade" <$> secs)

-- | Begin\/continue playing.
play :: MonadMPD m => Maybe PLIndex -> m ()
play Nothing        = getResponse_  "play"
play (Just (Pos x)) = getResponse_ ("play"   <$> x)
play (Just (ID x))  = getResponse_ ("playid" <$> x)

-- | Pause playing.
pause :: MonadMPD m => Bool -> m ()
pause = getResponse_ . ("pause" <$>)

-- | Stop playing.
stop :: MonadMPD m => m ()
stop = getResponse_ "stop"

-- | Play the next song.
next :: MonadMPD m => m ()
next = getResponse_ "next"

-- | Play the previous song.
previous :: MonadMPD m => m ()
previous = getResponse_ "previous"

-- | Seek to some point in a song.
-- Seeks in current song if no position is given.
seek :: MonadMPD m => Maybe PLIndex -> Seconds -> m ()
seek (Just (Pos x)) time = getResponse_ ("seek" <$> x <++> time)
seek (Just (ID x)) time = getResponse_ ("seekid" <$> x <++> time)
seek Nothing time = do
    st <- status
    unless (stState st == Stopped) (seek (stSongID st) time)

-- | Set random playing.
random :: MonadMPD m => Bool -> m ()
random = getResponse_ . ("random" <$>)

-- | Set repeating.
repeat :: MonadMPD m => Bool -> m ()
repeat = getResponse_ . ("repeat" <$>)

-- | Set single mode
single :: MonadMPD m => Bool -> m ()
single = getResponse_ . ("single" <$>)

-- | Set consume mode
consume :: MonadMPD m => Bool -> m ()
consume = getResponse_ . ("consume" <$>)

-- | Set the volume (0-100 percent).
setVolume :: MonadMPD m => Int -> m ()
setVolume = getResponse_ . ("setvol" <$>)

-- | Set the replay gain mode.
replayGainMode :: MonadMPD m => ReplayGainMode -> m ()
replayGainMode = getResponse_ . ("replay_gain_mode" <$>)

-- | Get the replay gain options.
replayGainStatus :: MonadMPD m => m [String]
replayGainStatus = getResponse "replay_gain_status"

--
-- Miscellaneous commands
--

-- | Clear the current error message in status.
clearError :: MonadMPD m => m ()
clearError = getResponse_ "clearerror"

-- | Retrieve a list of available commands.
commands :: MonadMPD m => m [String]
commands = liftM takeValues (getResponse "commands")

-- | Retrieve a list of unavailable (due to access restrictions) commands.
notCommands :: MonadMPD m => m [String]
notCommands = liftM takeValues (getResponse "notcommands")

-- | Retrieve a list of available song metadata.
tagTypes :: MonadMPD m => m [String]
tagTypes = liftM takeValues (getResponse "tagtypes")

-- | Retrieve a list of supported urlhandlers.
urlHandlers :: MonadMPD m => m [String]
urlHandlers = liftM takeValues (getResponse "urlhandlers")

-- | Retreive a list of decoder plugins with associated suffix and mime types.
decoders :: MonadMPD m => m [(String, [(String, String)])]
decoders = (takeDecoders . toAssocList) `liftM` getResponse "decoders"
    where
        takeDecoders [] = []
        takeDecoders ((_, p):xs) =
            let (info, rest) = break (((==) "plugin") . fst) xs
            in (p, info) : takeDecoders rest

-- XXX should the password be quoted? Change "++" to "<$>" if so.  If
--     it should, it also needs to be fixed in N.M.Core.
-- | Send password to server to authenticate session.
-- Password is sent as plain text.
password :: MonadMPD m => String -> m ()
password = getResponse_ . ("password " ++)

-- | Check that the server is still responding.
ping :: MonadMPD m => m ()
ping = getResponse_ "ping"

-- | Get server statistics.
stats :: MonadMPD m => m Stats
stats = getResponse "stats" >>= runParser parseStats

-- | Get the server's status.
status :: MonadMPD m => m Status
status = getResponse "status" >>= runParser parseStatus

--
-- Extensions\/shortcuts.
--

-- | Like 'update', but returns the update job id.
updateId :: MonadMPD m => [Path] -> m Integer
updateId paths = liftM (read . head . takeValues) cmd
  where cmd = case paths of
                []  -> getResponse  "update"
                [x] -> getResponse ("update" <$> x)
                xs  -> getResponses $ map ("update" <$>) xs

-- | Toggles play\/pause. Plays if stopped.
toggle :: MonadMPD m => m ()
toggle = status >>= \st -> case stState st of Playing -> pause True
                                              _       -> play Nothing

-- | Add a list of songs\/folders to a playlist.
-- Should be more efficient than running 'add' many times.
addMany :: MonadMPD m => PlaylistName -> [Path] -> m ()
addMany _ [] = return ()
addMany "" [x] = add_ x
addMany plname [x] = playlistAdd_ plname x
addMany plname xs = getResponses (map cmd xs) >> return ()
    where cmd x = case plname of
                      "" -> "add" <$> x
                      pl -> "playlistadd" <$> pl <++> x

-- | Delete a list of songs from a playlist.
-- If there is a duplicate then no further songs will be deleted, so
-- take care to avoid them (see 'prune' for this).
deleteMany :: MonadMPD m => PlaylistName -> [PLIndex] -> m ()
deleteMany _ [] = return ()
deleteMany plname [(Pos x)] = playlistDelete plname x
deleteMany "" xs = getResponses (map cmd xs) >> return ()
    where cmd (Pos x) = "delete"   <$> x
          cmd (ID x)  = "deleteid" <$> x
deleteMany plname xs = getResponses (map cmd xs) >> return ()
    where cmd (Pos x) = "playlistdelete" <$> plname <++> x
          cmd _       = ""

-- | Returns all songs and directories that match the given partial
-- path name.
complete :: MonadMPD m => String -> m [Either Path Song]
complete path = do
    xs <- liftM matches . lsInfo $ dropFileName path
    case xs of
        [Left dir] -> complete $ dir ++ "/"
        _          -> return xs
    where
        matches = filter (isPrefixOf path . takePath)
        takePath = either id sgFilePath

-- | Crop playlist.
-- The bounds are inclusive.
-- If 'Nothing' is passed the cropping will leave your playlist alone
-- on that side.
-- Using 'ID' will automatically find the absolute playlist position and use
-- that as the cropping boundary.
crop :: MonadMPD m => Maybe PLIndex -> Maybe PLIndex -> m ()
crop x y = do
    pl <- playlistInfo Nothing
    let x' = case x of Just (Pos p) -> fromInteger p
                       Just (ID i)  -> fromMaybe 0 (findByID i pl)
                       Nothing      -> 0
        -- ensure that no songs are deleted twice with 'max'.
        ys = case y of Just (Pos p) -> drop (max (fromInteger p) x') pl
                       Just (ID i)  -> maybe [] (flip drop pl . max x' . (+1))
                                      (findByID i pl)
                       Nothing      -> []
    deleteMany "" . mapMaybe sgIndex $ take x' pl ++ ys
    where findByID i = findIndex ((==) i . (\(ID j) -> j) . fromJust . sgIndex)

-- | Remove duplicate playlist entries.
prune :: MonadMPD m => m ()
prune = findDuplicates >>= deleteMany ""

-- Find duplicate playlist entries.
findDuplicates :: MonadMPD m => m [PLIndex]
findDuplicates =
    liftM (map ((\(ID x) -> ID x) . fromJust . sgIndex) . flip dups ([],[])) $
        playlistInfo Nothing
    where dups [] (_, dup) = dup
          dups (x:xs) (ys, dup)
              | x `mSong` xs && not (x `mSong` ys) = dups xs (ys, x:dup)
              | otherwise                          = dups xs (x:ys, dup)
          mSong x = let m = sgFilePath x in any ((==) m . sgFilePath)

-- | List directories non-recursively.
lsDirs :: MonadMPD m => Path -> m [Path]
lsDirs path =
    liftM (extractEntries (const Nothing,const Nothing, Just)) $
        takeEntries =<< getResponse ("lsinfo" <$> path)

-- | List files non-recursively.
lsFiles :: MonadMPD m => Path -> m [Path]
lsFiles path =
    liftM (extractEntries (Just . sgFilePath, const Nothing, const Nothing)) $
        takeEntries =<< getResponse ("lsinfo" <$> path)

-- | List all playlists.
lsPlaylists :: MonadMPD m => m [PlaylistName]
lsPlaylists = liftM (extractEntries (const Nothing, Just, const Nothing)) $
                    takeEntries =<< getResponse "lsinfo"

-- | List the artists in the database.
listArtists :: MonadMPD m => m [Artist]
listArtists = liftM takeValues (getResponse "list artist")

-- | List the albums in the database, optionally matching a given
-- artist.
listAlbums :: MonadMPD m => Maybe Artist -> m [Album]
listAlbums artist = liftM takeValues $
                    getResponse ("list album" <$> fmap ("artist" <++>) artist)

-- | List the songs in an album of some artist.
listAlbum :: MonadMPD m => Artist -> Album -> m [Song]
listAlbum artist album = find (Artist =? artist <&> Album =? album)

-- | Retrieve the current playlist.
-- Equivalent to @playlistinfo Nothing@.
getPlaylist :: MonadMPD m => m [Song]
getPlaylist = playlistInfo Nothing

-- | Increase or decrease volume by a given percent, e.g.
-- 'volume 10' will increase the volume by 10 percent, while
-- 'volume (-10)' will decrease it by the same amount.
volume :: MonadMPD m => Int -> m ()
volume n = do
    current <- (fromIntegral . stVolume) `liftM` status
    setVolume . round $ (fromIntegral n / 100) * current + current

--
-- Miscellaneous functions.
--

-- Run getResponse but discard the response.
getResponse_ :: MonadMPD m => String -> m ()
getResponse_ x = getResponse x >> return ()

-- Get the lines of the daemon's response to a list of commands.
getResponses :: MonadMPD m => [String] -> m [String]
getResponses cmds = getResponse . concat $ intersperse "\n" cmds'
    where cmds' = "command_list_begin" : cmds ++ ["command_list_end"]

-- Helper that throws unexpected error if input is empty.
failOnEmpty :: MonadMPD m => [String] -> m [String]
failOnEmpty [] = throwError $ Unexpected "Non-empty response expected."
failOnEmpty xs = return xs

-- A wrapper for getResponse that fails on non-empty responses.
getResponse1 :: MonadMPD m => String -> m [String]
getResponse1 x = getResponse x >>= failOnEmpty

--
-- Parsing.
--

-- Run 'toAssocList' and return only the values.
takeValues :: [String] -> [String]
takeValues = snd . unzip . toAssocList

data EntryType
    = SongEntry Song
    | PLEntry   String
    | DirEntry  String
      deriving (Eq, Show)

-- Separate the result of an lsinfo\/listallinfo call into directories,
-- playlists, and songs.
takeEntries :: MonadMPD m => [String] -> m [EntryType]
takeEntries = mapM toEntry . splitGroups wrappers . toAssocList
    where
        toEntry xs@(("file",_):_)   = liftM SongEntry $ runParser parseSong xs
        toEntry (("directory",d):_) = return $ DirEntry d
        toEntry (("playlist",pl):_) = return $ PLEntry  pl
        toEntry _ = error "takeEntries: splitGroups is broken"
        wrappers = [("file",id), ("directory",id), ("playlist",id)]

-- Extract a subset of songs, directories, and playlists.
extractEntries :: (Song -> Maybe a, String -> Maybe a, String -> Maybe a)
               -> [EntryType] -> [a]
extractEntries (fSong,fPlayList,fDir) = mapMaybe f
    where
        f (SongEntry s) = fSong s
        f (PLEntry pl)  = fPlayList pl
        f (DirEntry d)  = fDir d

-- Build a list of song instances from a response.
takeSongs :: MonadMPD m => [String] -> m [Song]
takeSongs = mapM (runParser parseSong)
          . splitGroups [("file",id)]
          . toAssocList
