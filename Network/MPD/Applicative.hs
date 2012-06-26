{-# LANGUAGE DeriveFunctor, OverloadedStrings, TupleSections #-}
module Network.MPD.Applicative where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)

import           Network.MPD.Commands.Arg hiding (Command)
import           Network.MPD.Commands.Types
import           Network.MPD.Commands.Parse hiding (runParser)
import qualified Network.MPD.Commands.Parse as P
import           Network.MPD.Core hiding (getResponse)
import qualified Network.MPD.Core as Core
import           Network.MPD.Util


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


-- | Return everything until the next "list_OK".
getResponse :: Parser [ByteString]
getResponse = Parser $ \input -> case break (== "list_OK") input of
  (xs, ys) -> Right (xs, drop 1 ys)

unexpected :: [ByteString] -> Parser a
unexpected = fail . ("unexpected Response: " ++) . show

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

addId :: Path -> Maybe Position -> Command Id
addId path pos = Command p c
  where
    c = ["addid" <@> path <++> pos]
    p = do
      r <- getResponse
      case toAssocList r of
        [("Id", n)] -> maybe (unexpected r) (return . Id) (parseNum n)
        _         -> unexpected r

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
      xs  -> unlines (["command_list_ok_begin"] ++ xs ++ ["command_list_end"])
