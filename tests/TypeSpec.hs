{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TypeSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QC

import           Network.MPD.Commands.Types (Volume)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Volume arithmetic is closed over 0-100" $ do
        prop "for addition" prop_volume_add
        prop "for subtraction" prop_volume_sub
        prop "for multiplication" prop_volume_mul
        prop "for division" prop_volume_div

instance QC.Arbitrary Volume where
    arbitrary = QC.elements [0..100]

inRange :: Ord a => a -> a -> a -> Bool
inRange l h x = l <= x && x <= h

prop_volume_arith op cur new = inRange 0 100 ((cur :: Volume) `op` new)

prop_volume_add = prop_volume_arith (+)
prop_volume_sub = prop_volume_arith (-)
prop_volume_mul = prop_volume_arith (*)
prop_volume_div cur new = new /= 0 QC.==> prop_volume_arith div cur new
