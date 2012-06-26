{-# LANGUAGE DeriveFunctor, OverloadedStrings, TupleSections #-}
module Network.MPD.Applicative where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)

import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Parse hiding (runParser)
import qualified Network.MPD.Commands.Parse as P
import           Network.MPD.Core


newtype Parser a = Parser {runParser :: [ByteString] -> Either String (a, [ByteString])}
  deriving Functor

instance Monad Parser where
  fail      = Parser . const . Left
  return a  = Parser $ \input -> Right (a, input)
  p1 >>= p2 = Parser $ \input -> runParser p1 input >>= uncurry (runParser . p2)

instance Applicative Parser where
  pure  = return
  (<*>) = ap

liftParser :: ([ByteString] -> Either String a) -> Parser a
liftParser p = Parser $ \input -> case break (== "list_OK") input of
  (xs, ys) -> fmap (, drop 1 ys) (p xs)

data Command a = Command {
  commandParser  :: Parser a
, commandRequest :: [String]
} deriving Functor

instance Applicative Command where
  pure a = Command (pure a) []
  (Command p1 c1) <*> (Command p2 c2) = Command (p1 <*> p2) (c1 ++ c2)

currentSong :: Command (Maybe Song)
currentSong = Command (liftParser parseMaybeSong) ["currentsong"]

stats :: Command Stats
stats = Command (liftParser parseStats) ["stats"]

runCommand :: MonadMPD m => Command a -> m a
runCommand (Command p c) = getResponse command >>= (P.runParser $ \r ->
  case runParser p r of
    Left err      -> Left err
    Right (a, []) -> Right a
    Right (_, xs) -> Left ("superfluous input: " ++ show xs)
  )
  where
    command = case c of
      [x] -> x
      xs  -> unlines (["command_list_ok_begin"] ++ xs ++ ["command_list_end"])
