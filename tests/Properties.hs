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
                  ,("toAssoc / integrity",
                        mytest prop_toAssoc_integrity)
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
    arbitrary     = choose ('\0', '\128')

-- an assoc. string is a string of the form "key: value".
newtype AssocString = AS String
    deriving Show

instance Arbitrary AssocString where
    arbitrary = do
        key <- arbitrary
        val <- arbitrary
        return . AS $ key ++ ": " ++ val

newtype IntegralString = IS String
    deriving Show

instance Arbitrary IntegralString where
    arbitrary = fmap (IS . show) (arbitrary :: Gen Integer)

newtype BoolString = BS String
    deriving Show

instance Arbitrary BoolString where
    arbitrary = fmap BS $ elements ["1", "0"]

-- Positive integers.
newtype PosInt = PI Integer

instance Show PosInt where
    show (PI x) = show x

instance Arbitrary PosInt where
    arbitrary = (PI . abs) `fmap` arbitrary

-- Simple date representation, like "2004" and "1998".
newtype SimpleDateString = SDS String
    deriving Show

instance Arbitrary SimpleDateString where
    arbitrary = (SDS . show) `fmap` (arbitrary :: Gen PosInt)

-- Complex date representations, like "2004-20-30".
newtype ComplexDateString = CDS String
    deriving Show

instance Arbitrary ComplexDateString where
    arbitrary = do
        -- eww...
        [y,m,d] <- replicateM 3 (arbitrary :: Gen PosInt)
        return . CDS . concat . intersperse "-" $ map show [y,m,d]

prop_parseDate_simple :: SimpleDateString -> Bool
prop_parseDate_simple (SDS x) = isJust $ parseDate x

prop_parseDate_complex :: ComplexDateString -> Bool
prop_parseDate_complex (CDS x) = isJust $ parseDate x

prop_toAssoc_rev :: [AssocString] -> Bool
prop_toAssoc_rev x = toAssoc (fromAssoc r) == r
    where r = toAssoc (fromAS x)
          fromAssoc = map (\(a, b) -> a ++ ": " ++ b)

prop_toAssoc_integrity :: [AssocString] -> Bool
prop_toAssoc_integrity x = length (toAssoc $ fromAS x) == length x

fromAS :: [AssocString] -> [String]
fromAS s = [x | AS x <- s]

prop_parseBool_rev :: BoolString -> Bool
prop_parseBool_rev (BS x) = showBool (fromJust $ parseBool x) == x

prop_parseBool :: BoolString -> Bool
prop_parseBool (BS "1") = fromJust $ parseBool "1"
prop_parseBool (BS x)   = not (fromJust $ parseBool x)

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

prop_parseNum :: IntegralString -> Bool
prop_parseNum (IS xs@"")      = parseNum xs == Nothing
prop_parseNum (IS xs@('-':_)) = fromMaybe 0 (parseNum xs) <= 0
prop_parseNum (IS xs)         = fromMaybe 0 (parseNum xs) >= 0


--------------------------------------------------------------------------
-- Parsers
--------------------------------------------------------------------------

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
        date  <- abs `fmap` arbitrary
        len   <- abs `fmap` arbitrary
        track <- two $ abs `fmap` arbitrary
        disc  <- two $ abs `fmap` arbitrary
        idx   <- oneof [return Nothing
                       ,liftM (Just . Pos) $ abs `fmap` arbitrary
                       ,liftM (Just . ID)  $ abs `fmap` arbitrary]
        return $ Song { sgArtist = artist, sgAlbum = album, sgTitle = title
                      , sgFilePath = file, sgGenre = genre, sgName = name
                      , sgComposer = cmpsr, sgPerformer = prfmr, sgLength = len
                      , sgDate = date, sgTrack = track, sgDisc = Just disc
                      , sgIndex = idx }

prop_parseSong :: Song -> Bool
prop_parseSong s = Right s == (parseSong . toAssoc . lines $ display s)

instance Arbitrary Stats where
    arbitrary = do
        let posInt = abs `fmap` arbitrary
        [arts,albs,sngs,upt,plt,dbplt,dbupd] <- replicateM 7 posInt
        return $ Stats arts albs sngs upt plt dbplt dbupd

prop_parseStats :: Stats -> Bool
prop_parseStats s = Right s == (parseStats . lines $ display s)
