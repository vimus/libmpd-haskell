{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
module Properties (main) where

import Displayable

import Network.MPD.Commands.Parse
import Network.MPD.Commands.Types
import Network.MPD.Utils

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import Text.Printf
import Test.QuickCheck

main :: IO ()
main = do
    n <- (maybe 100 read . listToMaybe) `liftM` getArgs
    mapM_ (\(s, f) -> printf "%-25s : " s >> f n) tests
    where tests = [("splitGroups / reversible",
                        mytest prop_splitGroups_rev)
                  ,("splitGroups / integrity",
                        mytest prop_splitGroups_integrity)
                  ,("parseBool", mytest prop_parseBool)
                  ,("parseBool / reversible",
                        mytest prop_parseBool_rev)
                  ,("showBool", mytest prop_showBool)
                  ,("toAssoc / reversible",
                        mytest prop_toAssoc_rev)
                  ,("parseNum", mytest prop_parseNum)
                  ,("parseDate / simple",
                        mytest prop_parseDate_simple)
                  ,("parseDate / complex",
                        mytest prop_parseDate_complex)
                  ,("parseCount", mytest prop_parseCount)
                  ,("parseOutputs", mytest prop_parseOutputs)
                  ,("parseSong", mytest prop_parseSong)
                  ,("parseStats", mytest prop_parseStats)]

mytest :: Testable a => a -> Int -> IO ()
mytest a n = check defaultConfig { configMaxTest = n } a

instance Arbitrary Char where
    arbitrary = choose ('\0', '\128')

-- an assoc. string is a string of the form "key: value", followed by
-- the key and value separately.
data AssocString = AS String String String

instance Show AssocString where
    show (AS str _ _) = str

instance Arbitrary AssocString where
    arbitrary = do
        key <- filter    (/= ':') `fmap` arbitrary
        val <- dropWhile (== ' ') `fmap` arbitrary
        return $ AS (key ++ ": " ++ val) key val

newtype BoolString = BS String
    deriving Show

instance Arbitrary BoolString where
    arbitrary = BS `fmap` elements ["1", "0"]

-- Simple date representation, like "2004" and "1998".
newtype SimpleDateString = SDS String
    deriving Show

instance Arbitrary SimpleDateString where
    arbitrary = (SDS . show) `fmap` (positive :: Gen Integer)

-- Complex date representations, like "2004-20-30".
newtype ComplexDateString = CDS String
    deriving Show

instance Arbitrary ComplexDateString where
    arbitrary = do
        (y,m,d) <- three (positive :: Gen Integer)
        return . CDS . concat . intersperse "-" $ map show [y,m,d]

prop_parseDate_simple :: SimpleDateString -> Bool
prop_parseDate_simple (SDS x) = isJust $ parseDate x

prop_parseDate_complex :: ComplexDateString -> Bool
prop_parseDate_complex (CDS x) = isJust $ parseDate x

-- Conversion to an association list.
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

prop_splitGroups_rev :: [(String, String)] -> Property
prop_splitGroups_rev xs = not (null xs) ==>
    let wrappers = [(fst $ head xs, id)]
        r = splitGroups wrappers xs
    in r == splitGroups wrappers (concat r)

prop_splitGroups_integrity :: [(String, String)] -> Property
prop_splitGroups_integrity xs = not (null xs) ==>
    sort (concat $ splitGroups [(fst $ head xs, id)] xs) == sort xs

prop_parseNum :: Integer -> Bool
prop_parseNum x =
    case show x of
        (xs@"")      -> parseNum xs == Nothing
        (xs@('-':_)) -> fromMaybe 0 (parseNum xs) <= 0
        (xs)         -> fromMaybe 0 (parseNum xs) >= 0


--------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------

-- Generate a positive number.
positive :: (Arbitrary a, Num a) => Gen a
positive = abs `fmap` arbitrary

-- MPD fields can't contain newlines and the parser skips initial spaces.
field :: Gen String
field = (filter (/= '\n') . dropWhile isSpace) `fmap` arbitrary

instance Arbitrary Count where
    arbitrary = liftM2 Count arbitrary arbitrary

prop_parseCount :: Count -> Bool
prop_parseCount c = Right c == (parseCount . lines $ display c)

instance Arbitrary Device where
    arbitrary = liftM3 Device arbitrary field arbitrary

prop_parseOutputs :: [Device] -> Bool
prop_parseOutputs ds =
    Right ds == (parseOutputs . lines $ concatMap display ds)

instance Arbitrary Song where
    arbitrary = do
        [file,artist,album,title,genre,name,cmpsr,prfmr] <- replicateM 8 field
        date  <- positive
        len   <- positive
        track <- two positive
        disc  <- two positive
        idx   <- oneof [return Nothing
                       ,(Just . Pos) `fmap` positive
                       ,(Just . ID)  `fmap` positive]
        return $ Song { sgArtist = artist, sgAlbum = album, sgTitle = title
                      , sgFilePath = file, sgGenre = genre, sgName = name
                      , sgComposer = cmpsr, sgPerformer = prfmr, sgLength = len
                      , sgDate = date, sgTrack = track, sgDisc = Just disc
                      , sgIndex = idx }

prop_parseSong :: Song -> Bool
prop_parseSong s = Right s == (parseSong . toAssocList . lines $ display s)

instance Arbitrary Stats where
    arbitrary =
        return Stats `ap` positive `ap` positive `ap` positive
                     `ap` positive `ap` positive `ap` positive `ap` positive

prop_parseStats :: Stats -> Bool
prop_parseStats s = Right s == (parseStats . lines $ display s)
