-- | Module    : Network.MPD.Query
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : unportable
--
-- Query interface.

module Network.MPD.Query (Query, (=?), (<&>), anything) where

import Network.MPD.Arg
import Network.MPD.Types

import Data.Monoid

-- | An interface for creating MPD queries.
--
-- For example, to match any song where the value of artist is \"Foo\", we
-- use:
--
-- > Artist =? "Foo"
--
-- We can also compose queries, thus narrowing the search. For example, to
-- match any song where the value of artist is \"Foo\" and the value of album
-- is \"Bar\", we use:
--
-- > Artist =? "Foo" <&> Album =? "Bar"
newtype Query = Query [Match] deriving Show

-- A single query clause, comprising a metadata key and a desired value.
data Match = Match Meta String

instance Show Match where
    show (Match meta query) = show meta ++ " \"" ++ query ++ "\""
    showList xs _ = unwords $ map show xs

instance Monoid Query where
    mempty  = Query []
    Query a `mappend` Query b = Query (a ++ b)

instance MPDArg Query where
    prep = foldl (<++>) (Args []) . f
        where f (Query ms) = map (\(Match m q) -> Args [show m] <++> q) ms

-- | An empty query. Matches anything.
anything :: Query
anything = mempty

-- | Create a query.
(=?) :: Meta -> String -> Query
m =? s = Query [Match m s]

-- | Combine queries.
infixr 6 <&>
(<&>) :: Query -> Query -> Query
(<&>) = mappend
