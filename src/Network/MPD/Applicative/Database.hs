{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Database
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachifm@fastmail.fm
Stability   : stable
Portability : unportable

The music database.
-}

module Network.MPD.Applicative.Database where

import qualified Network.MPD.Commands.Arg as Arg
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Query
import           Network.MPD.Util
import           Network.MPD.Commands.Types
import           Network.MPD.Applicative.Internal
import           Network.MPD.Applicative.Util

-- | Get a count of songs and their total playtime that exactly match the
-- query.
count :: Query -> Command Count
count q = Command (liftParser parseCount) ["count" <@> q]

-- | Find songs matching the query exactly.
find :: Query -> Command [Song]
find q = Command p ["find" <@> q]
    where
        p :: Parser [Song]
        p = liftParser takeSongs

-- | Like 'find' but adds the results to the current playlist.
findAdd :: Query -> Command ()
findAdd q = Command emptyResponse ["findadd" <@> q]

-- | Lists all tags of the specified type.
--
-- Note that the optional artist value is only ever used if the
-- metadata type is 'Album', and is then taken to mean that the albums
-- by that artist be listed.
list :: Metadata -> Maybe Artist -> Command [Value]
list m q = Command p c
    where
        p = map Value . takeValues <$> getResponse
        c = case m of
                Album -> ["list Album" <@> q]
                _     -> ["list" <@> m]

-- | List all songs and directories in a database path.
listAll :: Path -> Command [Path]
listAll path = Command p ["listall" <@> path]
    where
        p :: Parser [Path]
        p = map (Path . snd) . filter ((== "file") . fst)
            . toAssocList <$> getResponse

-- Internal helper
lsInfo' :: Arg.Command -> Path -> Command [LsResult]
lsInfo' cmd path = Command p [cmd <@> path]
    where
        p :: Parser [LsResult]
        p = liftParser takeEntries

-- | Same as 'listAll' but also returns metadata.
listAllInfo :: Path -> Command [LsResult]
listAllInfo = lsInfo' "listallinfo"

-- | List the contents of a database directory.
lsInfo :: Path -> Command [LsResult]
lsInfo = lsInfo' "lsinfo"

-- | Read comments from the file at the specified path.
readComments :: Path -> Command [(String, String)]
readComments uri = Command p ["readcomments" <@> uri]
  where p = map decodePair . toAssocList <$> getResponse

-- | Like 'find' but with inexact matching.
search :: Query -> Command [Song]
search q = Command p ["search" <@> q]
    where
        p :: Parser [Song]
        p = liftParser takeSongs

-- | Like 'search' but adds the results to the current playlist.
--
-- Since MPD 0.17.
searchAdd :: Query -> Command ()
searchAdd q = Command emptyResponse ["searchadd" <@> q]

-- | Like 'searchAdd' but adds results to the named playlist.
--
-- Since MPD 0.17.
searchAddPl :: PlaylistName -> Query -> Command ()
searchAddPl pl q = Command emptyResponse ["searchaddpl" <@> pl <++> q]

-- | Update the music database.
-- If no path is supplied, the entire database is updated.
update :: Maybe Path -> Command Integer
update = update_ "update"

-- | Like 'update' but also rescan unmodified files.
rescan :: Maybe Path -> Command Integer
rescan = update_ "rescan"

-- A helper for 'update' and 'rescan.
update_ :: Arg.Command -> Maybe Path -> Command Integer
update_ cmd mPath = Command p [cmd <@> mPath]
    where
        p :: Parser Integer
        p = do
            r <- getResponse
            case toAssocList r of
                [("updating_db", id_)] -> maybe (unexpected r)
                                                return
                                                (parseNum id_)
                _                      -> unexpected r
