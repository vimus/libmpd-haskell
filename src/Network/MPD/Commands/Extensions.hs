{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Extensions
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
Stability   : unstable
Portability : unportable

Extensions and shortcuts to the standard MPD command set.
-}

module Network.MPD.Commands.Extensions where

import           Network.MPD.Core
import           Network.MPD.Commands
import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.CurrentPlaylist as A
import qualified Network.MPD.Applicative.StoredPlaylists as A

import           Control.Monad (liftM)
import           Data.Traversable (for)
import           Data.Foldable (for_)

-- | This is exactly the same as `update`.
updateId :: MonadMPD m => Maybe Path -> m Integer
updateId = update
{-# DEPRECATED updateId "use `update` instead" #-}

-- | Toggles play\/pause. Plays if stopped.
toggle :: MonadMPD m => m ()
toggle = status >>= \st -> case stState st of Playing -> pause True
                                              _       -> play Nothing

-- | Add a list of songs\/folders to a playlist.
-- Should be more efficient than running 'add' many times.
addMany :: MonadMPD m => PlaylistName -> [Path] -> m ()
addMany plname xs = A.runCommand (for_ xs cmd)
    where cmd | plname == "" = A.add
              | otherwise    = A.playlistAdd plname

-- | Recursive 'addId'. For directories, it will use the given position
-- for the first file in the directory and use the successor for the remaining
-- files. It returns a list of playlist ids for the songs added.
addIdMany :: MonadMPD m => Path -> Maybe Position -> m [Id]
addIdMany x (Just p) = do
    fs <- listAll x
    let fs' = map (\(a, b) -> (a, Just b)) $ zip fs [p ..]
    A.runCommand $ for fs' (uncurry A.addId)
addIdMany x Nothing = do
    fs <- listAll x
    A.runCommand $ for fs (`A.addId` Nothing)

-- | Like 'add' but returns a list of the files added.
addList :: MonadMPD m => Path -> m [Path]
addList x = add x >> listAll x
{-# DEPRECATED addList "will be removed in a future version" #-}

-- | Like 'playlistAdd' but returns a list of the files added.
playlistAddList :: MonadMPD m => PlaylistName -> Path -> m [Path]
playlistAddList plname path = playlistAdd plname path >> listAll path
{-# DEPRECATED playlistAddList "will be removed in a future version" #-}

{-
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
-}

-- | List the artists in the database.
listArtists :: MonadMPD m => m [Artist]
listArtists = list Artist Nothing

-- | List the albums in the database, optionally matching a given
-- artist.
listAlbums :: MonadMPD m => Maybe Artist -> m [Album]
listAlbums = list Album

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
    setVolume . round $ (fromIntegral n / (100 :: Double)) * current + current
