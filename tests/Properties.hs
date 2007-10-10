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
                        mytest prop_splitGroups_integrity)]

mytest :: Testable a => a -> Int -> IO ()
mytest a n = check defaultConfig { configMaxTest = n } a

instance Arbitrary Char where
    arbitrary     = choose ('\0', '\128')
    coarbitrary c = variant (ord c `rem` 4)

prop_splitGroups_idem :: [(String, String)] -> Bool
prop_splitGroups_idem xs =
    let r = splitGroups xs in r == splitGroups (concat r)

prop_splitGroups_integrity :: [(String, String)] -> Bool
prop_splitGroups_integrity xs = sort (concat $ splitGroups xs) == sort xs
