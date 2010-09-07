-- | Module    : Network.MPD.Utils
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Utilities.

module Network.MPD.Utils (
    parseDate, parseIso8601, parseNum, parseFrac,
    parseBool, showBool, breakChar, toAssoc,
    toAssocList, splitGroups
    ) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Time.Format (ParseTime, parseTime)
import System.Locale (defaultTimeLocale)

-- Break a string by character, removing the separator.
breakChar :: Char -> String -> (String, String)
breakChar c s = let (x, y) = break (== c) s in (x, drop 1 y)

-- XXX: need a more robust date parser.
-- Parse a date value.
-- > parseDate "2008" = Just 2008
-- > parseDate "2008-03-01" = Just 2008
parseDate :: String -> Maybe Int
parseDate = parseNum . takeWhile isDigit

-- Parse date in iso 8601 format
parseIso8601 :: (ParseTime t) => String -> Maybe t
parseIso8601 = parseTime defaultTimeLocale "%FT%TZ"

-- Parse a positive or negative integer value, returning 'Nothing' on failure.
parseNum :: (Read a, Integral a) => String -> Maybe a
parseNum s = do
    [(x, "")] <- return (reads s)
    return x

-- Parse C style floating point value, returning 'Nothing' on failure.
parseFrac :: (Fractional a, Read a) => String -> Maybe a
parseFrac s =
    case s of
        "nan"  -> return $ read "NaN"
        "inf"  -> return $ read "Infinity"
        "-inf" -> return $ read "-Infinity"
        _      -> do [(x, "")] <- return $ reads s
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

-- Break a string into an key-value pair, separating at the first ':'.
toAssoc :: String -> (String, String)
toAssoc x = (k, dropWhile (== ' ') $ drop 1 v)
    where
        (k,v) = break (== ':') x

toAssocList :: [String] -> [(String, String)]
toAssocList = map toAssoc

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
    fromMaybe (splitGroups wrappers xs) $ do
        f <- k `lookup` wrappers
        let (us,vs) = break (\(k',_) -> k' `elem` map fst wrappers) xs
        return $ f (x:us) : splitGroups wrappers vs
