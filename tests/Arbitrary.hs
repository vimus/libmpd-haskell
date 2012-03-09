{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wwarn -fno-warn-orphans -fno-warn-missing-methods -XFlexibleInstances #-}

-- | This module contains Arbitrary instances for various types.

module Arbitrary
    ( AssocString(..)
    , BoolString(..)
    , YearString(..)
    , DateString(..)
    , positive, field
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (liftM2, liftM3, replicateM)
import           Data.Char (isSpace)
import           Data.List (intersperse)
import qualified Data.Map as M
import           Data.Time
import           Test.QuickCheck

import           Network.MPD.Commands.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

instance Arbitrary ByteString where
  arbitrary = UTF8.fromString <$> arbitrary

-- No longer provided by QuickCheck 2
two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

-- Generate a positive number.
positive :: (Arbitrary a, Num a) => Gen a
positive = abs <$> arbitrary

possibly :: Gen a -> Gen (Maybe a)
possibly m = arbitrary >>= bool (Just <$> m) (return Nothing)
    where bool thenE elseE b = if b then thenE else elseE

-- MPD fields can't contain newlines and the parser skips initial spaces.
field :: Gen String
field = (filter (/= '\n') . dropWhile isSpace) <$> arbitrary

fieldBS :: Gen ByteString
fieldBS = UTF8.fromString <$> field

-- Orphan instances for built-in types
instance Arbitrary (M.Map Metadata [Value]) where
    arbitrary = do
        size <- choose (1, 1000)
        vals <- replicateM size (listOf1 arbitrary)
        keys <- replicateM size arbitrary
        return $ M.fromList (zip keys vals)

instance Arbitrary Value where
    arbitrary = Value <$> fieldBS

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
    arbitrary = secondsToDiffTime <$> positive

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

-- an assoc. string is a string of the form "key: value", followed by
-- the key and value separately.
data AssocString = AS ByteString ByteString ByteString

instance Show AssocString where
    show (AS str _ _) = UTF8.toString str

instance Arbitrary AssocString where
    arbitrary = do
        key <- filter    (/= ':') <$> arbitrary
        val <- dropWhile (== ' ') <$> arbitrary
        return $ AS (UTF8.fromString (key ++ ": " ++ val))
                    (UTF8.fromString key)
                    (UTF8.fromString val)

newtype BoolString = BS ByteString
    deriving Show

instance Arbitrary BoolString where
    arbitrary = BS <$> elements ["1", "0"]

-- Simple date representation, like "2004" and "1998".
newtype YearString = YS ByteString
    deriving Show

instance Arbitrary YearString where
    arbitrary = YS . UTF8.fromString . show <$> (positive :: Gen Integer)

-- Complex date representations, like "2004-20-30".
newtype DateString = DS ByteString
    deriving Show

instance Arbitrary DateString where
    arbitrary = do
        (y,m,d) <- three (positive :: Gen Integer)
        return . DS . UTF8.fromString . concat . intersperse "-" $ map show [y,m,d]

instance Arbitrary Count where
    arbitrary = liftM2 Count arbitrary arbitrary

instance Arbitrary Device where
    arbitrary = liftM3 Device arbitrary field arbitrary

instance Arbitrary Id where
    arbitrary = Id <$> arbitrary

instance Arbitrary Song where
    arbitrary = Song <$> arbitrary
                     <*> arbitrary
                     <*> possibly arbitrary
                     <*> positive
                     <*> possibly arbitrary
                     <*> possibly positive

instance Arbitrary Path where
    arbitrary = Path <$> fieldBS

instance Arbitrary Stats where
    arbitrary = Stats <$> positive <*> positive <*> positive <*> positive
                      <*> positive <*> positive <*> positive

instance Arbitrary Metadata where
    arbitrary = elements [minBound .. maxBound]
