{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Network.MPD.Applicative.Database
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

The music database.
-}

module Network.MPD.Applicative.Database where

import           Control.Applicative

import qualified Network.MPD.Commands.Arg as Arg
import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Query
import           Network.MPD.Commands.Util hiding (takeEntries, takeSongs)
import           Network.MPD.Util
import           Network.MPD.Commands.Types
import           Network.MPD.Applicative
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

-- XXX: note this is probably incorrect.
-- list {TYPE} [ARTIST] where ARTIST is used only when TYPE is album and
-- specifies to list albums by an artist.
-- | Lists all tags of the specified type.
list :: Metadata -> Query -> Command [Value]
list m q = Command p ["list" <@> m <++> q]
    where
        p :: Parser [Value]
        p = map Value . takeValues <$> getResponse
{-# WARNING list "this command does not do what you think" #-}

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

-- | Like 'find' but with inexact matching.
search :: Query -> Command [Song]
search q = Command p ["search" <@> q]
    where
        p :: Parser [Song]
        p = liftParser takeSongs

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
