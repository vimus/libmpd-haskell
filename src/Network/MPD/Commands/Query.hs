{- |
Module      : Network.MPD.Commands.Query
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : Joachim Fasting <joachifm@fastmail.fm>
Stability   : unstable
Portability : unportable

Query interface.
-}

module Network.MPD.Commands.Query
  ( Query
  , (=?)
  , (<&>)
  , (/=?)
  , anything
  )
where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Types


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
-- > Artist =? "Foo" <> Album =? "Bar"
data Query = Query [Match] | Filter (NonEmpty Expr)
  deriving Show

-- A single query clause, comprising a metadata key and a desired value.
data Match = Match Metadata Value

instance Show Match where
  show (Match meta query) = show meta ++ " \"" ++ toString query ++ "\""
  showList xs _ = unwords $ fmap show xs

data Expr = Exact Match | ExactNot Match | ExprNot Expr
instance Show Expr where
  show (Exact (Match meta query)) =
    "( " ++ show meta ++ " == " ++ "\\\"" ++ toString query ++ "\\\"" ++ " )"
  show (ExactNot (Match meta query)) =
    "( " ++ show meta ++ " != " ++ "\\\"" ++ toString query ++ "\\\"" ++ " )"
  show (ExprNot expr) = "!" ++ show expr

instance Monoid Query where
  mempty = Query []
  Query  a  `mappend` Query  b  = Query (a ++ b)
  Query  [] `mappend` Filter b  = Filter b
  Filter a  `mappend` Query  [] = Filter a
  Query  a  `mappend` Filter b  = Filter (fromList (Exact <$> a) <> b)
  Filter a  `mappend` Query  b  = Filter (a <> fromList (Exact <$> b))
  Filter a  `mappend` Filter b  = Filter (a <> b)

instance Semigroup Query where
  (<>) = mappend

instance MPDArg Query where
  prep (Query ms) =
    foldl (<++>) (Args []) (fmap (\(Match m q) -> Args [show m] <++> q) ms)
  prep (Filter (e :| es)) = Args ["\"( " ++ expression ++ " )\""]
    where expression = foldr (\a b -> show a ++ " AND " ++ b) (show e) es
-- | An empty query. Matches anything.
anything :: Query
anything = mempty

-- | Create a query.
(=?) :: Metadata -> Value -> Query
m =? s = Query [Match m s]

-- | Create a negative search
(/=?) :: Metadata -> Value -> Query
m /=? s = Filter (ExactNot (Match m s) :| [])

-- | Combine queries.
infixr 6 <&>
(<&>) :: Query -> Query -> Query
(<&>) = mappend
{-# DEPRECATED (<&>) "will be removed in the next major version of libmpd, use `(<>)` instead" #-}
