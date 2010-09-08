-- | Module    : Network.MPD.Commands.Extensions
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Extensions and shortcuts to the standard MPD command set.

module Network.MPD.Commands.Extensions where

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
