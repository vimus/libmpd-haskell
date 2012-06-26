{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Database
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : LGPL-2 (see LICENSE)

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

The music database.
-}

module Network.MPD.Commands.Database
    ( count
    , find
    , findAdd
    , list
    , listAll
    , listAllInfo
    , lsInfo
    , search
    , update
    , rescan
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

import           Prelude hiding (read)

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
