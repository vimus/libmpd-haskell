{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Commands.Database
Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2012
License     : MIT (see LICENSE)

Maintainer  : joachifm@fastmail.fm
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
    , readComments
    , search
    , searchAdd
    , searchAddPl
    , update
    , rescan
    ) where

import qualified Network.MPD.Applicative.Internal as A
import qualified Network.MPD.Applicative.Database as A
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Types
import           Network.MPD.Core

-- | Count the number of entries matching a query.
count :: MonadMPD m => Query -> m Count
count = A.runCommand . A.count

-- | Search the database for entries exactly matching a query.
find :: MonadMPD m => Query -> m [Song]
find = A.runCommand . A.find

-- | Adds songs matching a query to the current playlist.
findAdd :: MonadMPD m => Query -> m ()
findAdd = A.runCommand . A.findAdd

-- | List all tags of the specified type of songs that that satisfy the query.
--
-- @since 0.10.0.0
list :: MonadMPD m
     => Metadata -- ^ Metadata to list
     -> Query -> m [Value]
list m = A.runCommand . A.list m

-- | List the songs (without metadata) in a database directory recursively.
listAll :: MonadMPD m => Path -> m [Path]
listAll = A.runCommand . A.listAll

-- | Recursive 'lsInfo'.
listAllInfo :: MonadMPD m => Path -> m [LsResult]
listAllInfo = A.runCommand . A.listAllInfo

-- | Non-recursively list the contents of a database directory.
lsInfo :: MonadMPD m => Path -> m [LsResult]
lsInfo = A.runCommand . A.lsInfo

-- | Read comments from file at path.
readComments :: MonadMPD m => Path -> m [(String, String)]
readComments = A.runCommand . A.readComments

-- | Search the database using case insensitive matching.
search :: MonadMPD m => Query -> m [Song]
search = A.runCommand . A.search

-- | Like 'search' but adds the results to the current playlist.
--
-- @since 0.10.0.0
searchAdd :: MonadMPD m => Query -> m ()
searchAdd = A.runCommand . A.searchAdd

-- | Like 'searchAdd' but adds results to the named playlist.
--
-- @since 0.10.0.0
searchAddPl :: MonadMPD m => PlaylistName -> Query -> m ()
searchAddPl pl = A.runCommand . A.searchAddPl pl

-- | Update the server's database.
--
-- If no path is given, the whole library will be scanned.  Unreadable or
-- non-existent paths are silently ignored.
--
-- The update job id is returned.
update :: MonadMPD m => Maybe Path -> m Integer
update = A.runCommand . A.update

-- | Like 'update' but also rescans unmodified files.
rescan :: MonadMPD m => Maybe Path -> m Integer
rescan = A.runCommand . A.rescan
