{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}

-- | Module    : Network.MPD.Commands.Arg
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : LGPL (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- Prepare command arguments.

module Network.MPD.Commands.Arg (Command, Args(..), MPDArg(..), (<++>), (<$>)) where

import           Network.MPD.Util (showBool)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String

-- | Arguments for getResponse are accumulated as strings in values of
-- this type after being converted from whatever type (an instance of
-- MPDArg) they were to begin with.
newtype Args = Args [String]
    deriving Show

-- | A uniform interface for argument preparation
-- The basic idea is that one should be able
-- to magically prepare an argument for use with
-- an MPD command, without necessarily knowing/\caring
-- how it needs to be represented internally.
class Show a => MPDArg a where
    prep :: a -> Args
    -- Note that because of this, we almost
    -- never have to actually provide
    -- an implementation of 'prep'
    prep = Args . return . show

-- | Groups together arguments to getResponse.
infixl 3 <++>
(<++>) :: (MPDArg a, MPDArg b) => a -> b -> Args
x <++> y = Args $ xs ++ ys
    where Args xs = prep x
          Args ys = prep y

newtype Command = Command String
  deriving IsString

-- | Converts a command name and a string of arguments into the string
-- to hand to getResponse.
infix 2 <$>
(<$>) :: (MPDArg a) => Command -> a -> String
Command x <$> y = unwords $ x : filter (not . null) y'
    where Args y' = prep y

instance MPDArg Args where prep = id

instance MPDArg String where
    -- We do this to avoid mangling
    -- non-ascii characters with 'show'
    prep x = Args ['"' : x ++ "\""]

instance MPDArg ByteString where
    prep = prep . UTF8.toString

instance (MPDArg a) => MPDArg (Maybe a) where
    prep Nothing = Args []
    prep (Just x) = prep x

instance (MPDArg a, MPDArg b) => MPDArg (a, b) where
    prep (x, y) = Args [show x ++ ":" ++ show y]

instance MPDArg Int
instance MPDArg Integer
instance MPDArg Bool where prep = Args . return . showBool
