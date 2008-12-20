-- | Module    : Network.MPD.Utils
-- Copyright   : (c) Ben Sinclair 2005-2008
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : Haskell 98
--
-- Utilities.

module Network.MPD.Utils (
    parseDate, parseNum, parseBool, showBool,
    breakChar, toAssoc, splitGroups
    ) where

import Data.Char (isDigit)

-- Break a string by character, removing the separator.
breakChar :: Char -> String -> (String, String)
breakChar c s = let (x, y) = break (== c) s in (x, drop 1 y)

-- XXX: need a more robust date parser.
-- Parse a date value.
-- > parseDate "2008" = Just 2008
-- > parseDate "2008-03-01" = Just 2008
parseDate :: String -> Maybe Int
parseDate = parseNum . takeWhile isDigit

-- Parse a positive or negative integer value, returning 'Nothing' on failure.
parseNum :: (Read a, Integral a) => String -> Maybe a
parseNum s = do
    [(x, "")] <- return (reads s)
    return x

-- Inverts 'parseBool'.
showBool :: Bool -> String
showBool x = if x then "1" else "0"

-- Parse a boolean response value.
parseBool :: String -> Maybe Bool
parseBool s = case take 1 s of
                  "1" -> Just True
                  "0" -> Just False
                  _   -> Nothing

-- Break up a list of strings into an assoc. list, separating at
-- the first ':'.
toAssoc :: [String] -> [(String, String)]
toAssoc = map f
    where f x = let (k,v) = break (== ':') x in
                (k,dropWhile (== ' ') $ drop 1 v)

-- Takes an assoc. list with recurring keys and groups each cycle of
-- keys with their values together. There can be several keys that
-- begin cycles, being listed as the first tuple component in the
-- first parameter. When a cycle finishes all of its pairs are passed
-- to the key's associated function (the second tuple component) and
-- the result appended to the resulting list.
--
-- > splitGroups [(1,id),(5,id)]
-- >             [(1,'a'),(2,'b'),(5,'c'),(6,'d'),(1,'z'),(2,'y'),(3,'x')] ==
-- >     [[(1,'a'),(2,'b')],[(5,'c'),(6,'d')],[(1,'z'),(2,'y'),(3,'x')]]
splitGroups :: Eq a => [(a,[(a,b)] -> c)] -> [(a, b)] -> [c]
splitGroups [] _ = []
splitGroups _ [] = []
splitGroups wrappers (x@(k,_):xs) =
    maybe (splitGroups wrappers xs) id $ do
        f <- k `lookup` wrappers
        let (us,vs) = break (\(k',_) -> k' `elem` map fst wrappers) xs
        return $ (f $ x:us) : splitGroups wrappers vs
