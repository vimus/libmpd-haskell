{-# LANGUAGE PatternGuards, OverloadedStrings #-}

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
    next, pause, play, playId, previous, seek, seekId, stop,

    -- * The current playlist
    add, addId, clear, delete, deleteId, move, moveId, playlist, playlistId,
    playlistFind, playlistInfo, playlistSearch, plChanges, plChangesPosId, shuffle, swap,
    swapId,

    -- * Stored playlist
    listPlaylist, listPlaylistInfo, listPlaylists, load,
    playlistAdd, playlistClear, playlistDelete, playlistMove, rename, rm,
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
    ) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Util
import           Network.MPD.Core
import           Network.MPD.Util

import           Control.Monad (liftM)
import           Control.Monad.Error (throwError)
import           Prelude hiding (repeat, read)

import qualified Data.ByteString.UTF8 as UTF8
import           Data.ByteString (ByteString)

--
-- Querying MPD's status
--

-- | Clear the current error message in status.
clearError :: MonadMPD m => m ()
clearError = getResponse_ "clearerror"

-- | Get the currently playing song.
currentSong :: (Functor m, MonadMPD m) => m (Maybe Song)
currentSong = getResponse "currentsong" >>= runParser parseMaybeSong . toAssocList

-- | Wait until there is a noteworthy change in one or more of MPD's
-- susbystems.
--
-- The first argument is a list of subsystems that should be considered.  An
-- empty list specifies that all subsystems should be considered.
--
-- A list of subsystems that have noteworthy changes is returned.
--
-- Note that running this command will block until either 'idle' returns or is
-- cancelled by 'noidle'.
idle :: MonadMPD m => [Subsystem] -> m [Subsystem]
idle subsystems =
    mapM f =<< toAssocList `liftM` getResponse ("idle" <@> foldr (<++>) (Args []) subsystems)
    where
        f ("changed", system) =
            case system of
                "database"        -> return DatabaseS
                "update"          -> return UpdateS
                "stored_playlist" -> return StoredPlaylistS
                "playlist"        -> return PlaylistS
                "player"          -> return PlayerS
                "mixer"           -> return MixerS
                "output"          -> return OutputS
                "options"         -> return OptionsS
                k                 -> fail ("Unknown subsystem: " ++ UTF8.toString k)
        f x                       =  fail ("idle: Unexpected " ++ show x)

-- | Cancel 'idle'.
noidle :: MonadMPD m => m ()
noidle = getResponse_ "noidle"

-- | Get server statistics.
stats :: MonadMPD m => m Stats
stats = getResponse "stats" >>= runParser parseStats

-- | Get the server's status.
status :: MonadMPD m => m Status
status = getResponse "status" >>= runParser parseStatus

--
-- Playback options
--

-- | Set consume mode
consume :: MonadMPD m => Bool -> m ()
consume = getResponse_ . ("consume" <@>)

-- | Set crossfading between songs.
crossfade :: MonadMPD m => Seconds -> m ()
crossfade secs = getResponse_ ("crossfade" <@> secs)

-- | Set random playing.
random :: MonadMPD m => Bool -> m ()
random = getResponse_ . ("random" <@>)

-- | Set repeating.
repeat :: MonadMPD m => Bool -> m ()
repeat = getResponse_ . ("repeat" <@>)

-- | Set the volume (0-100 percent).
setVolume :: MonadMPD m => Int -> m ()
setVolume = getResponse_ . ("setvol" <@>)

-- | Set single mode
single :: MonadMPD m => Bool -> m ()
single = getResponse_ . ("single" <@>)

-- | Set the replay gain mode.
replayGainMode :: MonadMPD m => ReplayGainMode -> m ()
replayGainMode = getResponse_ . ("replay_gain_mode" <@>)

-- | Get the replay gain options.
replayGainStatus :: MonadMPD m => m [String]
replayGainStatus = map UTF8.toString `liftM` getResponse "replay_gain_status"

--
-- Controlling playback
--

-- | Play the next song.
next :: MonadMPD m => m ()
next = getResponse_ "next"

-- | Pause playing.
pause :: MonadMPD m => Bool -> m ()
pause = getResponse_ . ("pause" <@>)

-- | Begin\/continue playing.
play :: MonadMPD m => Maybe Position -> m ()
play (Just pos) = getResponse_ ("play" <@> pos)
play _          = getResponse_  "play"

-- | Play a file with given id.
playId :: MonadMPD m => Id -> m ()
playId id' = getResponse_ ("playid" <@> id')

-- | Play the previous song.
previous :: MonadMPD m => m ()
previous = getResponse_ "previous"

-- | Seek to some point in a song.
seek :: MonadMPD m => Position -> Seconds -> m ()
seek pos time = getResponse_ ("seek" <@> pos <++> time)

-- | Seek to some point in a song (id version)
seekId :: MonadMPD m => Id -> Seconds -> m ()
seekId id' time = getResponse_ ("seekid" <@> id' <++> time)

-- | Stop playing.
stop :: MonadMPD m => m ()
stop = getResponse_ "stop"

--
-- The current playlist
--

-- This might do better to throw an exception than silently return 0.
-- | Like 'add', but returns a playlist id.
addId :: MonadMPD m => Path -> Maybe Position -- ^ Optional playlist position
      -> m Id
addId p pos = liftM (parse parseNum Id (Id 0) . snd . head . toAssocList)
              $ getResponse1 ("addid" <@> p <++> pos)

-- | Add a song (or a whole directory) to the current playlist.
add :: MonadMPD m => Path -> m ()
add path = getResponse_ ("add" <@> path)

-- | Clear the current playlist.
clear :: MonadMPD m => m ()
clear = getResponse_ "clear"

-- | Remove a song from the current playlist.
delete :: MonadMPD m => Position -> m ()
delete pos = getResponse_ ("delete" <@> pos)

-- | Remove a song from the current playlist.
deleteId :: MonadMPD m => Id -> m ()
deleteId id' = getResponse_ ("deleteid" <@> id')

-- | Move a song to a given position in the current playlist.
move :: MonadMPD m => Position -> Position -> m ()
move pos to = getResponse_ ("move" <@> pos <++> to)

-- | Move a song from (songid) to (playlist index) in the playlist. If to is
-- negative, it is relative to the current song in the playlist (if there is one).
moveId :: MonadMPD m => Id -> Position -> m ()
moveId id' to = getResponse_ ("moveid" <@> id' <++> to)

-- | Retrieve file paths and positions of songs in the current playlist.
-- Note that this command is only included for completeness sake; it's
-- deprecated and likely to disappear at any time, please use 'playlistInfo'
-- instead.
playlist :: MonadMPD m => m [(Position, Path)]
playlist = mapM f =<< getResponse "playlist"
    where f s | (pos, name) <- breakChar ':' s
              , Just pos'   <- parseNum pos
              = return (pos', Path name)
              | otherwise = throwError . Unexpected $ show s

-- | Search for songs in the current playlist with strict matching.
playlistFind :: MonadMPD m => Query -> m [Song]
playlistFind q = takeSongs =<< getResponse ("playlistfind" <@> q)

-- | Retrieve metadata for songs in the current playlist.
playlistInfo :: MonadMPD m => Maybe (Position, Position) -> m [Song]
playlistInfo range = takeSongs =<< getResponse ("playlistinfo" <@> range)

-- | Displays a list of songs in the playlist.
-- If id is specified, only its info is returned.
{-# WARNING playlistId "this function does not do what it looks like, and we will probably remove it!" #-}
playlistId :: MonadMPD m => Maybe Id -> m [Song]
-- FIXME: playlistinfo is used with an id here, but playlistinfo either takes a
-- range or a position, but never an id!
playlistId id' = takeSongs =<< getResponse ("playlistinfo" <@> id')

-- | Search case-insensitively with partial matches for songs in the
-- current playlist.
playlistSearch :: MonadMPD m => Query -> m [Song]
playlistSearch q = takeSongs =<< getResponse ("playlistsearch" <@> q)

-- | Retrieve a list of changed songs currently in the playlist since
-- a given playlist version.
plChanges :: MonadMPD m => Integer -> m [Song]
plChanges version = takeSongs =<< getResponse ("plchanges" <@> version)

-- | Like 'plChanges' but only returns positions and ids.
plChangesPosId :: MonadMPD m => Integer -> m [(Position, Id)]
plChangesPosId plver =
    getResponse ("plchangesposid" <@> plver) >>=
    mapM f . splitGroups ["cpos"] . toAssocList
    where f xs | [("cpos", x), ("Id", y)] <- xs
               , Just (x', y') <- pair parseNum (x, y)
               = return (x', Id y')
               | otherwise = throwError . Unexpected $ show xs

-- | Shuffle the playlist.
shuffle :: MonadMPD m => Maybe (Position, Position) -- ^ Optional range (start, end)
        -> m ()
shuffle range = getResponse_ ("shuffle" <@> range)

-- | Swap the positions of two songs.
swap :: MonadMPD m => Position -> Position -> m ()
swap pos1 pos2 = getResponse_ ("swap" <@> pos1 <++> pos2)

-- | Swap the positions of two songs (Id version
swapId :: MonadMPD m => Id -> Id -> m ()
swapId id1 id2 = getResponse_ ("swapid" <@> id1 <++> id2)

--
-- Stored playlists
--

-- | Retrieve a list of files in a given playlist.
listPlaylist :: MonadMPD m => PlaylistName -> m [Path]
listPlaylist plname =
    (map Path . takeValues) `liftM` getResponse ("listplaylist" <@> plname)

-- | Retrieve metadata for files in a given playlist.
listPlaylistInfo :: MonadMPD m => PlaylistName -> m [Song]
listPlaylistInfo plname =
    takeSongs =<< getResponse ("listplaylistinfo" <@> plname)

-- | Retreive a list of stored playlists.
listPlaylists :: MonadMPD m => m [PlaylistName]
listPlaylists = (map PlaylistName . go [] . toAssocList) `liftM` getResponse "listplaylists"
    where
        -- After each playlist name we get a timestamp
        go acc [] = acc
        go acc ((_, b):_:xs) = go (b : acc) xs
        go _ _ = error "listPlaylists: bug"

-- | Load an existing playlist.
load :: MonadMPD m => PlaylistName -> m ()
load plname = getResponse_ ("load" <@> plname)

-- | Add a song (or a whole directory) to a stored playlist.
-- Will create a new playlist if the one specified does not already exist.
playlistAdd :: MonadMPD m => PlaylistName -> Path -> m ()
playlistAdd plname path = getResponse_ ("playlistadd" <@> plname <++> path)

-- | Clear a playlist. If the specified playlist does not exist, it will be
-- created.
playlistClear :: MonadMPD m => PlaylistName -> m ()
playlistClear = getResponse_ . ("playlistclear" <@>)

-- | Remove a song from a playlist.
playlistDelete :: MonadMPD m => PlaylistName
               -> Position -- ^ Playlist position
               -> m ()
playlistDelete name pos = getResponse_ ("playlistdelete" <@> name <++> pos)

-- | Move a song to a given position in the playlist specified.
playlistMove :: MonadMPD m => PlaylistName -> Id -> Position -> m ()
playlistMove name from to =
    getResponse_ ("playlistmove" <@> name <++> from <++> to)

-- | Rename an existing playlist.
rename :: MonadMPD m
       => PlaylistName -- ^ Original playlist
       -> PlaylistName -- ^ New playlist name
       -> m ()
rename plname new = getResponse_ ("rename" <@> plname <++> new)

-- | Delete existing playlist.
rm :: MonadMPD m => PlaylistName -> m ()
rm plname = getResponse_ ("rm" <@> plname)

-- | Save the current playlist.
save :: MonadMPD m => PlaylistName -> m ()
save plname = getResponse_ ("save" <@> plname)

--
-- The music database
--

-- | Count the number of entries matching a query.
count :: MonadMPD m => Query -> m Count
count query = getResponse ("count" <@>  query) >>= runParser parseCount

-- | Search the database for entries exactly matching a query.
find :: MonadMPD m => Query -> m [Song]
find query = getResponse ("find" <@> query) >>= takeSongs

-- | Adds songs matching a query to the current playlist.
findAdd :: MonadMPD m => Query -> m ()
findAdd q = getResponse_ ("findadd" <@> q)

-- | List all tags of the specified type.
list :: MonadMPD m
     => Metadata -- ^ Metadata to list
     -> Query -> m [Value]
list mtype query = (map Value . takeValues) `liftM` getResponse ("list" <@> mtype <++> query)


-- | List the songs (without metadata) in a database directory recursively.
listAll :: MonadMPD m => Path -> m [Path]
listAll path = liftM (map (Path . snd) . filter ((== "file") . fst) . toAssocList)
                     (getResponse $ "listall" <@> path)

-- Helper for lsInfo and listAllInfo.
lsInfo' :: MonadMPD m => Command -> Path -> m [LsResult]
lsInfo' cmd path = getResponse (cmd <@> path) >>= takeEntries

-- | Recursive 'lsInfo'.
listAllInfo :: MonadMPD m => Path -> m [LsResult]
listAllInfo = lsInfo' "listallinfo"

-- | Non-recursively list the contents of a database directory.
lsInfo :: MonadMPD m => Path -> m [LsResult]
lsInfo = lsInfo' "lsinfo"

-- | Search the database using case insensitive matching.
search :: MonadMPD m => Query -> m [Song]
search query = getResponse ("search" <@> query) >>= takeSongs

-- | Update the server's database.
--
-- If no path is given, the whole library will be scanned.  Unreadable or
-- non-existent paths are silently ignored.
--
-- The update job id is returned.
update :: MonadMPD m => Maybe Path -> m Integer
update = update_ "update"

-- | Like 'update' but also rescans unmodified files.
rescan :: MonadMPD m => Maybe Path -> m Integer
rescan = update_ "rescan"

-- A helper for `update` and `rescan`.
update_ :: MonadMPD m => Command -> Maybe Path -> m Integer
update_ cmd mPath = do
    r <- getResponse (cmd <@> mPath)
    case toAssocList r of
        [("updating_db", id_)] -> return (read id_)
        _                      -> throwError . Unexpected $ show r

--
-- Stickers
--

-- | Reads a sticker value for the specified object.
stickerGet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> m [String]
stickerGet typ uri name = (map UTF8.toString . takeValues) `liftM` getResponse ("sticker get" <@> typ <++> uri <++> name)

-- | Adds a sticker value to the specified object.
stickerSet :: MonadMPD m => ObjectType
           -> String -- ^ Object URI
           -> String -- ^ Sticker name
           -> String -- ^ Sticker value
           -> m ()
stickerSet typ uri name value =
    getResponse_ ("sticker set" <@> typ <++> uri <++> name <++> value)

-- | Delete a sticker value from the specified object.
stickerDelete :: MonadMPD m => ObjectType
              -> String -- ^ Object URI
              -> String -- ^ Sticker name
              -> m ()
stickerDelete typ uri name =
    getResponse_ ("sticker delete" <@> typ <++> uri <++> name)

-- an internal helper function
decodePair :: (ByteString, ByteString) -> (String, String)
decodePair (x, y) = (UTF8.toString x, UTF8.toString y)

-- | Lists the stickers for the specified object.
stickerList :: MonadMPD m => ObjectType
            -> String -- ^ Object URI
            -> m [(String, String)] -- ^ Sticker name\/sticker value
stickerList typ uri =
    (map decodePair . toAssocList) `liftM` getResponse ("sticker list" <@> typ <++> uri)

-- | Searches the sticker database for stickers with the specified name, below
-- the specified path.
stickerFind :: MonadMPD m => ObjectType
            -> String -- ^ Path
            -> String -- ^ Sticker name
            -> m [(String, String)] -- ^ URI\/sticker value
stickerFind typ uri name =
    (map decodePair . toAssocList) `liftM`
    getResponse ("sticker find" <@> typ <++> uri <++> name)

--
-- Connection
--

-- XXX should the password be quoted? Change "++" to "<@>" if so.  If
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
disableOutput = getResponse_ . ("disableoutput" <@>)

-- | Turn on an output device.
enableOutput :: MonadMPD m => Int -> m ()
enableOutput = getResponse_ . ("enableoutput" <@>)

-- | Retrieve information for all output devices.
outputs :: MonadMPD m => m [Device]
outputs = getResponse "outputs" >>= runParser parseOutputs

--
-- Reflection
--

-- | Retrieve a list of available commands.
commands :: MonadMPD m => m [String]
commands = (map UTF8.toString . takeValues) `liftM` getResponse "commands"

-- | Retrieve a list of unavailable (due to access restrictions) commands.
notCommands :: MonadMPD m => m [String]
notCommands = (map UTF8.toString . takeValues) `liftM` getResponse "notcommands"

-- | Retrieve a list of available song metadata.
tagTypes :: MonadMPD m => m [String]
tagTypes = (map UTF8.toString . takeValues) `liftM` (getResponse "tagtypes")

-- | Retrieve a list of supported urlhandlers.
urlHandlers :: MonadMPD m => m [String]
urlHandlers = (map UTF8.toString . takeValues) `liftM` (getResponse "urlhandlers")

-- | Retreive a list of decoder plugins with associated suffix and mime types.
decoders :: MonadMPD m => m [(String, [(String, String)])]
decoders = (takeDecoders . toAssocList) `liftM` getResponse "decoders"
    where
        takeDecoders [] = []
        takeDecoders ((_, p):xs) =
            let (info, rest) = break ((==) "plugin" . fst) xs
            in (UTF8.toString p, map decodePair info) : takeDecoders rest
