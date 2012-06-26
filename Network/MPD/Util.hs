{-# LANGUAGE OverloadedStrings #-}
-- | Module    : Network.MPD.Util
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : MIT (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Utilities.

module Network.MPD.Util (
    parseDate, parseIso8601, formatIso8601, parseNum, parseFrac,
    parseBool, showBool, breakChar, parseTriple,
    toAssoc, toAssocList, splitGroups, read
    ) where

import           Control.Arrow

import           Data.Time.Format (ParseTime, parseTime, FormatTime, formatTime)
import           System.Locale (defaultTimeLocale)

import qualified Prelude
import           Prelude hiding        (break, take, drop, dropWhile, read)
import           Data.ByteString.Char8 (break, drop, dropWhile, ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A

-- | Like Prelude.read, but works with ByteString.
read :: Read a => ByteString -> a
read = Prelude.read . UTF8.toString

-- Break a string by character, removing the separator.
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar c = second (drop 1) . break (== c)

-- Parse a date value.
-- > parseDate "2008" = Just 2008
-- > parseDate "2008-03-01" = Just 2008
parseDate :: ByteString -> Maybe Int
parseDate = parseMaybe p
    where
        p = A.decimal <* A.skipMany (A.char '-' <|> A.digit)

-- Parse date in iso 8601 format
parseIso8601 :: (ParseTime t) => ByteString -> Maybe t
parseIso8601 = parseTime defaultTimeLocale iso8601Format . UTF8.toString

formatIso8601 :: FormatTime t => t -> String
formatIso8601 = formatTime defaultTimeLocale iso8601Format

iso8601Format :: String
iso8601Format = "%FT%TZ"

-- Parse a positive or negative integer value, returning 'Nothing' on failure.
parseNum :: (Read a, Integral a) => ByteString -> Maybe a
parseNum = parseMaybe (A.signed A.decimal)

-- Parse C style floating point value, returning 'Nothing' on failure.
parseFrac :: (Fractional a, Read a) => ByteString -> Maybe a
parseFrac = parseMaybe p
    where
        p = A.string "nan" *> pure (Prelude.read "NaN")
            <|> A.string "inf" *> pure (Prelude.read "Infinity")
            <|> A.string "-inf" *> pure (Prelude.read "-Infinity")
            <|> A.rational

-- Inverts 'parseBool'.
showBool :: IsString a => Bool -> a
-- FIXME: can we change the type to (Bool -> ByteString)?
showBool x = if x then "1" else "0"

-- Parse a boolean response value.
parseBool :: ByteString -> Maybe Bool
parseBool = parseMaybe p
    where
        p = A.char '1' *> pure True <|> A.char '0' *> pure False

-- Break a string into triple.
parseTriple :: Char -> (ByteString -> Maybe a) -> ByteString -> Maybe (a, a, a)
parseTriple c f s = let (u, u') = breakChar c s
                        (v, w)  = breakChar c u' in
    case (f u, f v, f w) of
        (Just a, Just b, Just c') -> Just (a, b, c')
        _                        -> Nothing

-- Break a string into a key-value pair, separating at the first ':'.
toAssoc :: ByteString -> (ByteString, ByteString)
toAssoc = second (dropWhile (== ' ') . drop 1) . break (== ':')

toAssocList :: [ByteString] -> [(ByteString, ByteString)]
toAssocList = map toAssoc

-- Takes an association list with recurring keys and groups each cycle of keys
-- with their values together.  There can be several keys that begin cycles,
-- (the elements of the first parameter).
splitGroups :: [ByteString] -> [(ByteString, ByteString)] -> [[(ByteString, ByteString)]]
splitGroups groupHeads = go
  where
    go []     = []
    go (x:xs) =
      let
        (ys, zs) = Prelude.break isGroupHead xs
      in
        (x:ys) : go zs

    isGroupHead = (`elem` groupHeads) . fst

-- A helper for running a Parser, turning errors into Nothing.
parseMaybe :: A.Parser a -> ByteString -> Maybe a
parseMaybe p s = either (const Nothing) Just $ A.parseOnly (p <* A.endOfInput) s
