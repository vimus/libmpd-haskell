{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-methods #-}

-- | This module contains Arbitrary instances for various types.

module Arbitrary
    ( AssocString(..)
    , BoolString(..)
    , YearString(..)
    , DateString(..)
    , positive, field
    ) where

import Control.Monad (ap, liftM2, liftM3, replicateM)
import Data.Char (isSpace)
import Data.List (intersperse)
import Test.QuickCheck

import Network.MPD.Commands.Types

-- No longer provided by QuickCheck 2
two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

-- Generate a positive number.
positive :: (Arbitrary a, Num a) => Gen a
positive = abs `fmap` arbitrary

-- MPD fields can't contain newlines and the parser skips initial spaces.
field :: Gen String
field = (filter (/= '\n') . dropWhile isSpace) `fmap` arbitrary

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
newtype YearString = YS String
    deriving Show

instance Arbitrary YearString where
    arbitrary = (YS . show) `fmap` (positive :: Gen Integer)

-- Complex date representations, like "2004-20-30".
newtype DateString = DS String
    deriving Show

instance Arbitrary DateString where
    arbitrary = do
        (y,m,d) <- three (positive :: Gen Integer)
        return . DS . concat . intersperse "-" $ map show [y,m,d]

instance Arbitrary Count where
    arbitrary = liftM2 Count arbitrary arbitrary

instance Arbitrary Device where
    arbitrary = liftM3 Device arbitrary field arbitrary

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
                      , sgIndex = idx, sgAux = [] }

instance Arbitrary Stats where
    arbitrary =
        return Stats `ap` positive `ap` positive `ap` positive
                     `ap` positive `ap` positive `ap` positive `ap` positive

instance Arbitrary Meta where
    arbitrary =
        oneof $ map return [Artist, Album, Title, Track, Disc
                           ,Name, Genre, Date
                           ,Composer, Performer, Filename, Any
                           ]
