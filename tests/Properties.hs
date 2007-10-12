{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where
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
                  ,("toAssoc / reversible",
                        mytest prop_toAssoc_rev)
                  ,("toAssoc / integrity",
                        mytest prop_toAssoc_integrity)
                  ,("parseNum", mytest prop_parseNum)]

mytest :: Testable a => a -> Int -> IO ()
mytest a n = check defaultConfig { configMaxTest = n } a

instance Arbitrary Char where
    arbitrary     = choose ('\0', '\128')
    coarbitrary c = variant (ord c `rem` 4)

-- an assoc. string is a string of the form "key: value".
newtype AssocString = AS String
    deriving Show

instance Arbitrary AssocString where
    arbitrary = do
        key <- arbitrary
        val <- arbitrary
        return . AS $ key ++ ": " ++ val
    coarbitrary = undefined

newtype IntegralString = IS String
    deriving Show

instance Arbitrary IntegralString where
    arbitrary = do
        xs <- sized $ \n -> replicateM (n `min` 15) $
                            oneof (map return ['0'..'9'])
        neg <- oneof [return True, return False]
        return $ IS (if neg then '-':xs else xs)
    coarbitrary = undefined

newtype BoolString = BS String
    deriving Show

instance Arbitrary BoolString where
    arbitrary = do
        v <- oneof [return True, return False]
        return . BS $ if v then "1" else "0"
    coarbitrary = undefined

prop_toAssoc_rev :: [AssocString] -> Bool
prop_toAssoc_rev x = toAssoc (fromAssoc r) == r
    where r = toAssoc (fromAS x)
          fromAssoc = map (\(a, b) -> a ++ ": " ++ b)

prop_toAssoc_integrity :: [AssocString] -> Bool
prop_toAssoc_integrity x = length (toAssoc $ fromAS x) == length x

fromAS :: [AssocString] -> [String]
fromAS s = [x | AS x <- s]

prop_parseBool :: BoolString -> Bool
prop_parseBool (BS "1") = parseBool "1"
prop_parseBool (BS x)   = not (parseBool x)

prop_showBool :: Bool -> Bool
prop_showBool True = showBool True == "1"
prop_showBool x    = showBool x == "0"

prop_splitGroups_rev :: [(String, String)] -> Bool
prop_splitGroups_rev xs =
    let r = splitGroups xs in r == splitGroups (concat r)

prop_splitGroups_integrity :: [(String, String)] -> Bool
prop_splitGroups_integrity xs = sort (concat $ splitGroups xs) == sort xs

prop_parseNum :: IntegralString -> Bool
prop_parseNum (IS xs@"")      = parseNum xs == 0
prop_parseNum (IS xs@('-':_)) = parseNum xs <= 0
prop_parseNum (IS xs)         = parseNum xs >= 0
