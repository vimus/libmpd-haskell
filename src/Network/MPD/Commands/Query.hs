{- |
Module      : Network.MPD.Commands.Query
Copyright   : (c) Joachim Fasting 2012
License     : MIT

Maintainer  : Joachim Fasting <joachifm@fastmail.fm>
Stability   : unstable
Portability : unportable

Query interface.
-}

module Network.MPD.Commands.Query (Query, (=?) ,(/=?), (%?), (~?), (/~?), qNot, qModSince, qFile, qBase, (<&>), anything) where

import           Network.MPD.Commands.Arg
import           Network.MPD.Commands.Types
import           Data.Time (UTCTime,formatTime,defaultTimeLocale)

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
data Query = Query [Match] | Filter Expr
  deriving Show

-- A single query clause, comprising a metadata key and a desired value.
data Match = Match Metadata Value

instance Show Match where
  show (Match meta query) = show meta ++ " \"" ++ toString query ++ "\""
  showList xs _ = unwords $ fmap show xs

data Expr = Exact Match
          | ExactNot Match
          | Contains Match
          | Regex Match
          | RegexNot Match
          | File FilePath
          | Base FilePath
          | ModifiedSince UTCTime
          | ExprNot Expr
          | ExprAnd Expr Expr
          | ExprEmpty
instance Show Expr where
 show (Exact (Match meta query)) = "(" ++ show meta ++ " == " ++
                                   "\\\"" ++ toString query ++ "\\\"" ++ ")"
 show (ExactNot (Match meta query)) = "(" ++ show meta ++ " != " ++
                                   "\\\"" ++ toString query ++ "\\\"" ++ ")"
 show (Contains (Match meta query)) = "(" ++ show meta ++ " contains " ++
                                   "\\\"" ++ toString query ++ "\\\"" ++ ")"
 show (Regex (Match meta query)) = "(" ++ show meta ++ " =~ " ++
                                   "\\\"" ++ toString query ++ "\\\"" ++ ")"
 show (RegexNot (Match meta query)) = "(" ++ show meta ++ " !~ " ++
                                   "\\\"" ++ toString query ++ "\\\"" ++ ")"
 show (File file) = "(file == " ++ "\\\"" ++ show file ++ "\\\"" ++ ")"
 show (Base dir) = "(base " ++ "\\\"" ++ show dir ++ "\\\"" ++ ")"
 show (ModifiedSince time) = "(modified-since " ++  "\\\"" ++
                           formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time ++
                           "\\\"" ++ ")"
 show (ExprNot expr) = "(!" ++ show expr ++ ")"
 show (ExprAnd e1 e2) = "(" ++ show e1 ++ " AND " ++ show e2 ++ ")"
 show ExprEmpty = ""

toExpr :: [Match] -> Expr
toExpr [] = ExprEmpty
toExpr (m:[]) = Exact m
toExpr (m:ms) = ExprAnd (Exact m) (toExpr ms)

instance Monoid Query where
    mempty  = Query []
    Query  a  `mappend` Query    b  = Query (a ++ b)
    Query  [] `mappend` Filter   b  = Filter b
    Filter a  `mappend` Query    [] = Filter a
    Query  a  `mappend` Filter   b  = Filter (ExprAnd (toExpr a) b)
    Filter a  `mappend` Query    b  = Filter (ExprAnd a (toExpr b))
    Filter a  `mappend` Filter   b  = Filter (a <> b)

instance Semigroup Query where
    (<>) = mappend
instance Semigroup Expr where
  ex1 <> ex2 = ExprAnd ex1 ex2

instance MPDArg Query where
    prep (Query ms) = foldl (<++>) (Args [])
                        (fmap (\(Match m q) -> Args [show m] <++> q) ms)
    prep (Filter expr) = Args ["\"" ++ show expr ++ "\""]

-- | An empty query. Matches anything.
anything :: Query
anything = mempty

-- | Create a query matching a tag with a value.
(=?) :: Metadata -> Value -> Query
m =? s = Query [Match m s]

-- | Create a query matching a tag with anything but a value.
-- requires MPD 0.21 or newer.
(/=?) :: Metadata -> Value -> Query
m /=? s = Filter (ExactNot (Match m s))

-- | Create a query for a tag containing a value.
-- requires MPD 0.21 or newer.
(%?) :: Metadata -> Value -> Query
m %? s = Filter (Contains (Match m s))

-- | Create a query matching a tag with regexp.
-- requires MPD 0.21 or newer.
(~?) :: Metadata -> Value -> Query
m ~? s = Filter (Regex (Match m s))

-- | Create a query matching a tag with anything but a regexp.
-- requires MPD 0.21 or newer.
(/~?) :: Metadata -> Value -> Query
m /~? s = Filter (RegexNot (Match m s))

-- | Negate a Query.
-- requires MPD 0.21 or newer.
qNot :: Query -> Query
qNot (Query ms) = Filter (ExprNot (toExpr ms))
qNot (Filter (ExprNot ex)) = Filter ex
qNot (Filter ex) = Filter (ExprNot ex)

-- | Create a query for songs modified since a date.
-- requires MPD 0.21 or newer.
qModSince :: UTCTime -> Query
qModSince time = Filter (ModifiedSince time)

-- | Create a query for the full song URI relative to the music directory.
-- requires MPD 0.21 or newer.
qFile :: FilePath -> Query
qFile file = Filter (File file)

-- | Limit the query to the given directory, relative to the music directory.
-- requires MPD 0.21 or newer.
qBase :: FilePath -> Query
qBase dir = Filter (Base dir)

-- | Combine queries.
infixr 6 <&>
(<&>) :: Query -> Query -> Query
(<&>) = mappend
{-# DEPRECATED (<&>) "will be removed in the next major version of libmpd, use `(<>)` instead" #-}
