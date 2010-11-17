{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}

-- | This module contains Arbitrary instances for various types.

module Arbitrary
    ( AssocString(..)
    , BoolString(..)
    , YearString(..)
    , DateString(..)
    , positive, field
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM2, liftM3, replicateM)
import Data.Char (isSpace)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Time
import Test.QuickCheck

import Network.MPD.Commands.Types

-- No longer provided by QuickCheck 2
two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

-- Generate a positive number.
positive :: (Arbitrary a, Num a) => Gen a
positive = abs <$> arbitrary

-- MPD fields can't contain newlines and the parser skips initial spaces.
field :: Gen String
field = (filter (/= '\n') . dropWhile isSpace) <$> arbitrary

-- Orphan instances for built-in types
instance (Ord key, Arbitrary key, Arbitrary val) => Arbitrary (M.Map key val) where
    arbitrary = do
        size <- choose (1, 1000)
        vals <- replicateM size arbitrary
        keys <- replicateM size arbitrary
        return $ M.fromList (zip keys vals)

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

-- an assoc. string is a string of the form "key: value", followed by
-- the key and value separately.
data AssocString = AS String String String

instance Show AssocString where
    show (AS str _ _) = str

instance Arbitrary AssocString where
    arbitrary = do
        key <- filter    (/= ':') <$> arbitrary
        val <- dropWhile (== ' ') <$> arbitrary
        return $ AS (key ++ ": " ++ val) key val

newtype BoolString = BS String
    deriving Show

instance Arbitrary BoolString where
    arbitrary = BS `fmap` elements ["1", "0"]

-- Simple date representation, like "2004" and "1998".
newtype YearString = YS String
    deriving Show

instance Arbitrary YearString where
    arbitrary = YS . show <$> (positive :: Gen Integer)

-- Complex date representations, like "2004-20-30".
newtype DateString = DS String
    deriving Show

instance Arbitrary DateString where
    arbitrary = do
        (y,m,d) <- three (positive :: Gen Integer)
        return . DS . concat . intersperse "-" $ map show [y,m,d]

instance Arbitrary Count where
    arbitrary = liftM2 Count arbitrary arbitrary

instance Arbitrary Output where
    arbitrary = liftM3 Output arbitrary field arbitrary

instance Arbitrary Song where
    arbitrary = do
        Song <$> field
             <*> arbitrary
             <*> arbitrary
             <*> positive
             <*> arbitrary

instance Arbitrary Stats where
    arbitrary = Stats <$> positive <*> positive <*> positive <*> positive
                      <*> positive <*> positive <*> positive

-- XXX: maybe derive Enum/Bounded
instance Arbitrary Metadata where
    arbitrary = elements [Artist, ArtistSort, Album, AlbumArtist,
                          AlbumArtistSort, Title, Track, Name, Genre,
                          Date, Composer, Performer, Comment, Disc,
                          MUSICBRAINZ_ARTISTID, MUSICBRAINZ_ALBUMID,
                          MUSICBRAINZ_ALBUMARTISTID, MUSICBRAINZ_TRACKID]
