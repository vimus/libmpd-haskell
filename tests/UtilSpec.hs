{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module UtilSpec (main, spec) where

import           Arbitrary
import           TestUtil

import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

import           Data.List (sort)
import           Data.Maybe (fromJust, isJust)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

import           Network.MPD.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "splitGroups" $ do
        it "breaks an association list into sublists" $ do
            splitGroups ["1", "5"]
                        [("1","a"),("2","b"),
                         ("5","c"),("6","d"),
                         ("1","z"),("2","y"),("3","x")]
            `shouldBe`
            [[("1","a"),("2","b")],
             [("5","c"),("6","d")],
             [("1","z"),("2","y"),("3","x")]]
        prop "is reversible" prop_splitGroups_rev
        prop "preserves input" prop_splitGroups_integrity

    describe "parseDate" $ do
        prop "simple year strings" prop_parseDate_simple
        prop "complex year strings" prop_parseDate_complex

    describe "toAssoc" $ do
        prop "is reversible" prop_toAssoc_rev

    describe "parseBool" $ do
        prop "parses boolean values" prop_parseBool
        prop "is reversible" prop_parseBool_rev

    describe "showBool" $ do
        prop "unparses boolean values" prop_showBool

    describe "parseNum" $ do
        prop "parses positive and negative integers" prop_parseNum


prop_parseDate_simple :: YearString -> Bool
prop_parseDate_simple (YS x) = isJust $ parseDate x

prop_parseDate_complex :: DateString -> Bool
prop_parseDate_complex (DS x) = isJust $ parseDate x

prop_toAssoc_rev :: AssocString -> Bool
prop_toAssoc_rev x = k == k' && v == v'
    where
        AS str k v = x
        (k',v') = toAssoc str

prop_parseBool_rev :: BoolString -> Bool
prop_parseBool_rev (BS x) = showBool (fromJust $ parseBool x) == x

prop_parseBool :: BoolString -> Bool
prop_parseBool (BS xs) =
    case parseBool xs of
        Nothing    -> False
        Just True  -> xs == "1"
        Just False -> xs == "0"

prop_showBool :: Bool -> Bool
prop_showBool True = showBool True == "1"
prop_showBool x    = showBool x == "0"

prop_splitGroups_rev :: [(ByteString, ByteString)] -> Property
prop_splitGroups_rev xs = not (null xs) ==>
    let wrappers = [fst $ head xs]
        r = splitGroups wrappers xs
    in r == splitGroups wrappers (concat r)

prop_splitGroups_integrity :: [(ByteString, ByteString)] -> Property
prop_splitGroups_integrity xs = not (null xs) ==>
    sort (concat $ splitGroups [fst $ head xs] xs) == sort xs

prop_parseNum :: Integer -> Bool
prop_parseNum x =
    case xs of
        '-':_ -> ((<= 0) `fmap` parseNum bs) == Just True
        _     -> ((>= 0) `fmap` parseNum bs) == Just True
    where
      xs = show x
      bs = UTF8.fromString xs
