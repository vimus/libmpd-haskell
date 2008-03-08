{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}
module Properties (main) where
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
                        mytest prop_parseDate_complex)]

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
    arbitrary = fmap BS $ oneof [return "1", return "0"]

-- Simple date representation, like "2004" and "1998".
newtype SimpleDateString = SDS String
    deriving Show

instance Arbitrary SimpleDateString where
    arbitrary = fmap (SDS . concatMap show) .
                replicateM 4 . oneof $ map return [0..9]

-- Complex date representations, like "2004-20-30".
newtype ComplexDateString = CDS String
    deriving Show

instance Arbitrary ComplexDateString where
    arbitrary = do
        -- eww...
        y <- replicateM 4 . oneof $ map return [0..9]
        let (m, d) = splitAt 2 y
        return . CDS . intercalate "-" $ map (concatMap show) [y, m, d]

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

prop_splitGroups_rev :: [(String, String)] -> Bool
prop_splitGroups_rev xs =
    let r = splitGroups xs in r == splitGroups (concat r)

prop_splitGroups_integrity :: [(String, String)] -> Bool
prop_splitGroups_integrity xs = sort (concat $ splitGroups xs) == sort xs

prop_parseNum :: IntegralString -> Bool
prop_parseNum (IS xs@"")      = parseNum xs == Nothing
prop_parseNum (IS xs@('-':_)) = fromMaybe 0 (parseNum xs) <= 0
prop_parseNum (IS xs)         = fromMaybe 0 (parseNum xs) >= 0
