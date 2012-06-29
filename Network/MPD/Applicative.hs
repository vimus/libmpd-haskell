{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Network.MPD.Applicative
Copyright   : (c) Simon Hengel 2012
License     : MIT

Maintainer  : joachim.fasting@gmail.com
Stability   : stable
Portability : unportable

Applicative MPD command interface.

This allows us to combine commands into command lists, as in

> (,,) <$> currentSong <*> stats <*> status

where the requests are automatically combined into a command list and
the result of each command passed to the consumer.
-}

module Network.MPD.Applicative
    ( Parser(..)
    , liftParser
    , getResponse
    , emptyResponse
    , unexpected
    , Command(..)
    , runCommand
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)

import qualified Network.MPD.Commands.Parse as P
import           Network.MPD.Core hiding (getResponse)
import qualified Network.MPD.Core as Core

-- | A line-oriented parser that returns a value along with any remaining input.
newtype Parser a
    = Parser { runParser :: [ByteString] -> Either String (a, [ByteString]) }
      deriving Functor

instance Monad Parser where
    fail      = Parser . const . Left
    return a  = Parser $ \input -> Right (a, input)
    p1 >>= p2 = Parser $ \input -> runParser p1 input >>= uncurry (runParser . p2)

instance Applicative Parser where
    pure  = return
    (<*>) = ap

-- | Convert a regular parser.
liftParser :: ([ByteString] -> Either String a) -> Parser a
liftParser p = Parser $ \input -> case break (== "list_OK") input of
    (xs, ys) -> fmap (, drop 1 ys) (p xs)

-- | Return everything until the next "list_OK".
getResponse :: Parser [ByteString]
getResponse = Parser $ \input -> case break (== "list_OK") input of
    (xs, ys) -> Right (xs, drop 1 ys)

-- | For commands returning an empty response.
emptyResponse :: Parser ()
emptyResponse = do
    r <- getResponse
    unless (null r) $
        unexpected r

-- | Fail with unexpected response.
unexpected :: [ByteString] -> Parser a
unexpected = fail . ("unexpected Response: " ++) . show

-- | A compound command, comprising a parser for the responses and a
-- combined request of an arbitrary number of commands.
data Command a = Command {
     commandParser  :: Parser a
   , commandRequest :: [String]
   } deriving Functor

instance Applicative Command where
    pure a = Command (pure a) []
    (Command p1 c1) <*> (Command p2 c2) = Command (p1 <*> p2) (c1 ++ c2)

-- | Execute a 'Command'.
runCommand :: MonadMPD m => Command a -> m a
runCommand (Command p c) = Core.getResponse command >>= (P.runParser $ \r ->
    case runParser p r of
        Left err      -> Left err
        Right (a, []) -> Right a
        Right (_, xs) -> Left ("superfluous input: " ++ show xs)
    )
    where
        command = case c of
            [x] -> x
            xs  -> unlines (["command_list_ok_begin"] ++ xs
                   ++ ["command_list_end"])
