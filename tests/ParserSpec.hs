{-# LANGUAGE StandaloneDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserSpec (main, spec) where

import           Arbitrary ()
import           Unparse

import           Test.Hspec.Monadic
import           Test.Hspec.HUnit ()
import           Test.Hspec.QuickCheck (prop)

import           Network.MPD.Commands.Parse
import           Network.MPD.Commands.Types
import           Network.MPD.Util hiding (read)

import qualified Data.ByteString.UTF8 as UTF8
import           Data.List
import qualified Data.Map as M
import           Data.Time

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseIso8601" $ do
        prop "parses dates in ISO8601 format" prop_parseIso8601

    describe "parseCount" $ do
        prop "parses counts" prop_parseCount

    describe "parseOutputs" $ do
        prop "parses outputs" prop_parseOutputs

    describe "parseSong" $ do
        prop "parses songs" prop_parseSong

    describe "parseStats" $ do
        prop "parses stats" prop_parseStats

-- This property also ensures, that (instance Arbitrary UTCTime) is sound.
-- Indeed, a bug in the instance declaration was the primary motivation to add
-- this property.
prop_parseIso8601 :: UTCTime -> Bool
prop_parseIso8601 t = Just t == (parseIso8601 . UTF8.fromString . formatIso8601) t

prop_parseCount :: Count -> Bool
prop_parseCount c = Right c == (parseCount . map UTF8.fromString . lines . unparse) c

prop_parseOutputs :: [Device] -> Bool
prop_parseOutputs ds =
    Right ds == (parseOutputs . map UTF8.fromString . lines . concatMap unparse) ds

deriving instance Ord Value

prop_parseSong :: Song -> Bool
prop_parseSong s = Right (sortTags s) == sortTags `fmap` (parseSong . toAssocList . map UTF8.fromString . lines . unparse) s
  where
    -- We consider lists of tag values equal if they contain the same elements.
    -- To ensure that two lists with the same elements are equal, we bring the
    -- elements in a deterministic order.
    sortTags song = song { sgTags = M.map sort $ sgTags song }

prop_parseStats :: Stats -> Bool
prop_parseStats s = Right s == (parseStats . map UTF8.fromString . lines . unparse) s
