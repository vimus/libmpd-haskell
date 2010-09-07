{-# LANGUAGE PatternGuards #-}

-- | Module    : Network.MPD.Commands
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Interface to the user commands supported by MPD.

module Network.MPD.Commands (
    -- * Command related data types
    module Network.MPD.Commands.Types,

    -- * Query interface
    module Network.MPD.Commands.Query,

    -- * Querying MPD's status
    clearError, currentSong, idle, noidle, status, stats,

    -- * Playback options
    consume, crossfade, random, repeat, setVolume, single, replayGainMode,
    replayGainStatus,

    -- * Controlling playback
    next, pause, play, previous, seek, stop,

    -- * The current playlist
    add, add_, addId, clear, delete, move, playlist, playlistFind,
    playlistInfo, playlistSearch, plChanges, plChangesPosId, shuffle, swap,

    -- * Stored playlist
    listPlaylist, listPlaylistInfo, listPlaylists, load, playlistAdd,
    playlistAdd_, playlistClear, playlistDelete, playlistMove, rename, rm,
    save,

    -- * The music database
    count, find, findAdd, list, listAll, listAllInfo, lsInfo, search, update,
    rescan,

    -- * Stickers
    stickerGet, stickerSet, stickerDelete, stickerList, stickerFind,

    -- * Connection
    close, kill, password, ping,

    -- * Audio output devices
    disableOutput, enableOutput, outputs,

    -- * Reflection
    commands, notCommands, tagTypes, urlHandlers, decoders,

    -- * Extensions\/shortcuts
    addMany, {-deleteMany, complete, crop, prune, lsDirs, lsFiles, lsPlaylists,-}
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
-- Querying MPD's status
--

-- | Clear the current error message in status.
clearError :: MonadMPD m => m ()
clearError = getResponse_ "clearerror"

-- | Get the current song.
currentSong :: MonadMPD m => m (Maybe Song)
currentSong = getResponse "currentsong" >>= takeSongs >>= return . listToMaybe

-- | Wait until there is a noteworthy change in one or more of MPD's
-- susbystems. Note that running this command will block until either 'idle'
-- returns or is cancelled by 'noidle'.
idle :: MonadMPD m => m [Subsystem]
idle =
    mapM (\("changed", system) -> case system of "database" -> return DatabaseS
                                                 "update"   -> return UpdateS
                                                 "stored_playlist" -> return StoredPlaylistS
                                                 "playlist" -> return PlaylistS
                                                 "player" -> return PlayerS
                                                 "mixer" -> return MixerS
                                                 "output" -> return OutputS
                                                 "options" -> return OptionsS
                                                 k -> fail ("Unknown subsystem: " ++ k))
         =<< toAssocList `liftM` getResponse "idle"

-- | Cancel 'idle'.
noidle :: MonadMPD m => m ()
noidle = getResponse_ "noidle"

-- | Get server statistics.
stats :: MonadMPD m => m Stats
stats = getResponse "stats" >>= return . parseStats . toAssocList

-- | Get the server's status.
status :: MonadMPD m => m Status
status = getResponse "status" >>= return . parseStatus . toAssocList

--
-- Playback options
--

-- | Set consume mode
consume :: MonadMPD m => Bool -> m ()
consume = getResponse_ . ("consume" <$>)

-- | Set crossfading between songs.
crossfade :: MonadMPD m => Seconds -> m ()
crossfade secs = getResponse_ ("crossfade" <$> secs)

-- | Set random playing.
random :: MonadMPD m => Bool -> m ()
random = getResponse_ . ("random" <$>)

-- | Set repeating.
repeat :: MonadMPD m => Bool -> m ()
repeat = getResponse_ . ("repeat" <$>)

-- | Set the volume (0-100 percent).
setVolume :: MonadMPD m => Int -> m ()
setVolume = getResponse_ . ("setvol" <$>)

-- | Set single mode
single :: MonadMPD m => Bool -> m ()
single = getResponse_ . ("single" <$>)

-- | Set the replay gain mode.
replayGainMode :: MonadMPD m => ReplayGainMode -> m ()
replayGainMode = getResponse_ . ("replay_gain_mode" <$>)

-- | Get the replay gain options.
replayGainStatus :: MonadMPD m => m [String]
replayGainStatus = getResponse "replay_gain_status"

--
-- Controlling playback
--

-- | Play the next song.
next :: MonadMPD m => m ()
next = getResponse_ "next"

-- | Pause playing.
pause :: MonadMPD m => Bool -> m ()
pause = getResponse_ . ("pause" <$>)

-- | Begin\/continue playing.
play :: MonadMPD m => Maybe Int -> m ()
play (Just pos) = getResponse_ ("play" <$> pos)
play _          = getResponse_  "play"

-- | Play a file with given id.
playId :: MonadMPD m => Int -> m ()
playId id' = getResponse_ ("playid" <$> id')

-- | Play the previous song.
previous :: MonadMPD m => m ()
previous = getResponse_ "previous"

-- | Seek to some point in a song.
seek :: MonadMPD m => Int -> Seconds -> m ()
seek pos time = getResponse_ ("seek" <$> pos <++> time)

-- | Seek to some point in a song (id version)
seekId :: MonadMPD m => Int -> Seconds -> m ()
seekId id' time = getResponse_ ("seekid" <$> id' <++> time)

-- | Stop playing.
stop :: MonadMPD m => m ()
stop = getResponse_ "stop"

--
-- The current playlist
--

-- This might do better to throw an exception than silently return 0.
-- | Like 'add', but returns a playlist id.
addId :: MonadMPD m => Path -> Maybe Integer -- ^ Optional playlist position
      -> m Int
addId p pos = liftM (parse parseNum id 0 . snd . head . toAssocList)
              $ getResponse1 ("addid" <$> p <++> pos)

-- | Like 'add_' but returns a list of the files added.
add :: MonadMPD m => Path -> m [Path]
add x = add_ x >> listAll x

-- | Add a song (or a whole directory) to the current playlist.
add_ :: MonadMPD m => Path -> m ()
add_ path = getResponse_ ("add" <$> path)

-- | Clear the current playlist.
clear :: MonadMPD m => m ()
clear = getResponse_ "clear"

-- | Remove a song from the current playlist.
delete :: MonadMPD m => Int -> m ()
delete pos = getResponse_ ("delete" <$> pos)

-- | Remove a song from the current playlist.
deleteId :: MonadMPD m => Int -> m ()
deleteId id' = getResponse_ ("deleteid" <$> id')

-- | Move a song to a given position in the current playlist.
move :: MonadMPD m => Int -> Int -> m ()
move pos to = getResponse_ ("move" <$> pos <++> to)

-- | Move a song from (songid) to (playlist index) in the playlist. If to is
-- negative, it is relative to the current song in the playlist (if there is one).
moveId :: MonadMPD m => Int -> Int -> m ()
moveId id' to = getResponse_ ("moveid" <$> id' <++> to)

-- | Retrieve file paths and positions of songs in the current playlist.
-- Note that this command is only included for completeness sake; it's
-- deprecated and likely to disappear at any time, please use 'playlistInfo'
-- instead.
playlist :: MonadMPD m => m [(Int, Path)]
playlist = mapM f =<< getResponse "playlist"
    where f s | (pos, name) <- breakChar ':' s
              , Just pos'   <- parseNum pos
              = return (pos', name)
              | otherwise = throwError . Unexpected $ show s

-- | Search for songs in the current playlist with strict matching.
playlistFind :: MonadMPD m => Query -> m [Song]
playlistFind q = takeSongs =<< getResponse ("playlistfind" <$> q)

-- | Retrieve metadata for songs in the current playlist.
playlistInfo :: MonadMPD m => Maybe (Int, Int) -> m [Song]
playlistInfo range = takeSongs =<< getResponse ("playlistinfo" <$> range)

-- | Displays a list of songs in the playlist.
-- If id is specified, only its info is returned.
playlistId :: MonadMPD m => Maybe Int -> m [Song]
playlistId id' = takeSongs =<< getResponse ("playlistinfo" <$> id')

-- | Search case-insensitively with partial matches for songs in the
-- current playlist.
playlistSearch :: MonadMPD m => Query -> m [Song]
playlistSearch q = takeSongs =<< getResponse ("playlistsearch" <$> q)

-- | Retrieve a list of changed songs currently in the playlist since
-- a given playlist version.
plChanges :: MonadMPD m => Integer -> m [Song]
plChanges version = takeSongs =<< getResponse ("plchanges" <$> version)

-- | Like 'plChanges' but only returns positions and ids.
plChangesPosId :: MonadMPD m => Integer -> m [(Int, Int)]
plChangesPosId plver =
    getResponse ("plchangesposid" <$> plver) >>=
    mapM f . splitGroups [("cpos",id)] . toAssocList
    where f xs | [("cpos", x), ("Id", y)] <- xs
               , Just (x', y') <- pair parseNum (x, y)
               = return (x', y')
               | otherwise = throwError . Unexpected $ show xs

-- | Shuffle the playlist.
shuffle :: MonadMPD m => Maybe (Int, Int) -- ^ Optional range (start, end)
        -> m ()
shuffle range = getResponse_ ("shuffle" <$> range)

-- | Swap the positions of two songs.
swap :: MonadMPD m => Int -> Int -> m ()
swap pos1 pos2 = getResponse_ ("swap" <$> pos1 <++> pos2)

-- | Swap the positions of two songs (Id version
swapId :: MonadMPD m => Int -> Int -> m ()
swapId id1 id2 = getResponse_ ("swapid" <$> id1 <++> id2)

--
-- Stored playlists
--

-- | Retrieve a list of files in a given playlist.
listPlaylist :: MonadMPD m => PlaylistName -> m [Path]
listPlaylist plname =
    liftM takeValues $ getResponse ("listplaylist" <$> plname)

-- | Retrieve metadata for files in a given playlist.
listPlaylistInfo :: MonadMPD m => PlaylistName -> m [Song]
listPlaylistInfo plname =
    takeSongs =<< getResponse ("listplaylistinfo" <$> plname)

-- | Retreive a list of stored playlists.
listPlaylists :: MonadMPD m => m [PlaylistName]
listPlaylists = (go [] . toAssocList) `liftM` getResponse "listplaylists"
    where
        -- After each playlist name we get a timestamp
        go acc [] = acc
        go acc ((_, b):_:xs) = go (b : acc) xs
        go _ _ = error "listPlaylists: bug"

-- | Load an existing playlist.
load :: MonadMPD m => PlaylistName -> m ()
load plname = getResponse_ ("load" <$> plname)

-- | Like 'playlistAdd' but returns a list of the files added.
playlistAdd :: MonadMPD m => PlaylistName -> Path -> m [Path]
playlistAdd plname path = playlistAdd_ plname path >> listAll path

-- | Add a song (or a whole directory) to a stored playlist.
-- Will create a new playlist if the one specified does not already exist.
playlistAdd_ :: MonadMPD m => PlaylistName -> Path -> m ()
playlistAdd_ plname path = getResponse_ ("playlistadd" <$> plname <++> path)

-- | Clear a playlist. If the specified playlist does not exist, it will be
-- created.
playlistClear :: MonadMPD m => PlaylistName -> m ()
playlistClear = getResponse_ . ("playlistclear" <$>)

-- | Remove a song from a playlist.
playlistDelete :: MonadMPD m => PlaylistName
               -> Integer -- ^ Playlist position
               -> m ()
playlistDelete name pos = getResponse_ ("playlistdelete" <$> name <++> pos)

-- | Move a song to a given position in the playlist specified.
playlistMove :: MonadMPD m => PlaylistName -> Integer -> Integer -> m ()
playlistMove name from to =
    getResponse_ ("playlistmove" <$> name <++> from <++> to)

-- | Rename an existing playlist.
rename :: MonadMPD m
       => PlaylistName -- ^ Original playlist
       -> PlaylistName -- ^ New playlist name
       -> m ()
rename plname new = getResponse_ ("rename" <$> plname <++> new)

-- | Delete existing playlist.
rm :: MonadMPD m => PlaylistName -> m ()
rm plname = getResponse_ ("rm" <$> plname)

-- | Save the current playlist.
save :: MonadMPD m => PlaylistName -> m ()
save plname = getResponse_ ("save" <$> plname)

--
-- The music database
--

-- | Count the number of entries matching a query.
count :: MonadMPD m => Query -> m Count
count query = getResponse ("count" <$>  query) >>= return . parseCount . toAssocList

-- | Search the database for entries exactly matching a query.
find :: MonadMPD m => Query -> m [Song]
find query = getResponse ("find" <$> query) >>= takeSongs

-- | Adds songs matching a query to the current playlist.
findAdd :: MonadMPD m => Query -> m ()
findAdd q = getResponse_ ("findadd" <$> q)

-- | List all metadata of metadata (sic).
list :: MonadMPD m
     => Metadata -- ^ Metadata to list
     -> Query -> m [String]
list mtype query = liftM takeValues $ getResponse ("list" <$> mtype <++> query)

-- | List the songs (without metadata) in a database directory recursively.
listAll :: MonadMPD m => Path -> m [Path]
listAll path = liftM (map snd . filter ((== "file") . fst) . toAssocList)
                     (getResponse $ "listall" <$> path)

-- | Recursive 'lsInfo'.
listAllInfo :: MonadMPD m => Path -> m [Song]
listAllInfo path = takeSongs =<< getResponse ("listallinfo" <$> path)

-- | Non-recursively list the contents of a database directory.
lsInfo :: MonadMPD m => Path -> m [Entry]
lsInfo path = takeEntries =<< getResponse ("lsinfo" <$> path)

-- | Search the database using case insensitive matching.
search :: MonadMPD m => Query -> m [Song]
search query = takeSongs =<< getResponse ("search" <$> query)

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
-- Stickers
--

-- | Reads a sticker value for the specified object.
stickerGet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> m [String]
stickerGet typ uri name = takeValues `liftM` getResponse ("sticker get" <$> typ <++> uri <++> name)

-- | Adds a sticker value to the specified object.
stickerSet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> String -- ^ Sticker value
           -> m ()
stickerSet typ uri name value =
    getResponse_ ("sticker set" <$> typ <++> uri <++> name <++> value)

-- | Delete a sticker value from the specified object.
stickerDelete :: MonadMPD m => ObjectType
              -> String -- ^ Object URI
              -> String -- ^ Sticker name
              -> m ()
stickerDelete typ uri name =
    getResponse_ ("sticker delete" <$> typ <++> uri <++> name)

-- | Lists the stickers for the specified object.
stickerList :: MonadMPD m => ObjectType
            -> String -- ^ Object URI
            -> m [(String, String)] -- ^ Sticker name\/sticker value
stickerList typ uri =
    toAssocList `liftM` getResponse ("sticker list" <$> typ <++> uri)

-- | Searches the sticker database for stickers with the specified name, below
-- the specified path.
stickerFind :: MonadMPD m => ObjectType
            -> String -- ^ Path
            -> String -- ^ Sticker name
            -> m [(String, String)] -- ^ URI\/sticker value
stickerFind typ uri name =
    toAssocList `liftM`
    getResponse ("sticker find" <$> typ <++> uri <++> name)

--
-- Connection
--

-- XXX should the password be quoted? Change "++" to "<$>" if so.  If
--     it should, it also needs to be fixed in N.M.Core.
-- | Send password to server to authenticate session.
-- Password is sent as plain text.
password :: MonadMPD m => String -> m ()
password = getResponse_ . ("password " ++)

-- | Check that the server is still responding.
ping :: MonadMPD m => m ()
ping = getResponse_ "ping"

--
-- Audio output devices
--

-- | Turn off an output device.
disableOutput :: MonadMPD m => Int -> m ()
disableOutput = getResponse_ . ("disableoutput" <$>)

-- | Turn on an output device.
enableOutput :: MonadMPD m => Int -> m ()
enableOutput = getResponse_ . ("enableoutput" <$>)

-- | Retrieve information for all output devices.
outputs :: MonadMPD m => m [Output]
outputs = getResponse "outputs" >>= return . parseOutputs . toAssocList

--
-- Reflection
--

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
            let (info, rest) = break ((==) "plugin" . fst) xs
            in (p, info) : takeDecoders rest

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
{- deleteMany :: MonadMPD m => PlaylistName -> [PLIndex] -> m ()
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
                    takeEntries =<< getResponse "lsinfo" -}

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
takeValues = map snd . toAssocList

-- Build a list of Song instances from a response.
takeSongs :: MonadMPD m => [String] -> m [Song]
takeSongs = return . parseSongs . toAssocList

-- Build a list of Entry instances from a response.
takeEntries :: MonadMPD m => [String] -> m [Entry]
takeEntries = return . parseEntries . toAssocList
