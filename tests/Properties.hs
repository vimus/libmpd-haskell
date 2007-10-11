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
    where tests = [("splitGroups / idempotent",
                        mytest prop_splitGroups_idem)
                  ,("splitGroups / integrity",
                        mytest prop_splitGroups_integrity)
                  ,("parseBool", mytest prop_parseBool)
                  ,("toAssoc / idempotent",
                        mytest prop_toAssoc_idem)
                  ,("toAssoc / integrity",
                        mytest prop_toAssoc_integrity)]

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

prop_toAssoc_idem :: [AssocString] -> Bool
prop_toAssoc_idem x = toAssoc (fromAssoc r) == r
    where r = toAssoc s
          fromAssoc = map (\(a, b) -> a ++ ": " ++ b)
          s = [y | AS y <- x]

prop_toAssoc_integrity :: [AssocString] -> Bool
prop_toAssoc_integrity x = let s = [y | AS y <- x] in length (toAssoc s) == length s

prop_parseBool :: Bool -> Bool
prop_parseBool x = parseBool (showBool x) == x

prop_splitGroups_idem :: [(String, String)] -> Bool
prop_splitGroups_idem xs =
    let r = splitGroups xs in r == splitGroups (concat r)

prop_splitGroups_integrity :: [(String, String)] -> Bool
prop_splitGroups_integrity xs = sort (concat $ splitGroups xs) == sort xs
