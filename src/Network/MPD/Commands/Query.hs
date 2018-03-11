{-# LANGUAGE CPP #-}

{- |
Module      : Network.MPD.Commands.Query
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : Joachim Fasting <joachifm@fastmail.fm>
Stability   : unstable
Portability : unportable

Query interface.
-}

module Network.MPD.Commands.Query (Query, (=?), (<&>), anything) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Types

import           Data.Monoid
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup
#endif

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
data Match = Match Metadata Value

instance Show Match where
    show (Match meta query) = show meta ++ " \"" ++ toString query ++ "\""
    showList xs _ = unwords $ map show xs

instance Monoid Query where
    mempty  = Query []
    Query a `mappend` Query b = Query (a ++ b)

#if MIN_VERSION_base(4,9,0)
instance Semigroup Query where
    (<>) = mappend
#endif

instance MPDArg Query where
    prep = foldl (<++>) (Args []) . f
        where f (Query ms) = map (\(Match m q) -> Args [show m] <++> q) ms

-- | An empty query. Matches anything.
anything :: Query
anything = mempty

-- | Create a query.
(=?) :: Metadata -> Value -> Query
m =? s = Query [Match m s]

-- | Combine queries.
infixr 6 <&>
(<&>) :: Query -> Query -> Query
(<&>) = mappend
