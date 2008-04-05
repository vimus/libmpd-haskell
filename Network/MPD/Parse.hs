module Network.MPD.Parse where

import Control.Monad.Error
import Network.MPD.Utils
import Network.MPD.Core (MPD, MPDError(Unexpected))

type Seconds = Integer

-- | Represents the result of running 'count'.
data Count =
    Count { cSongs    :: Integer -- ^ Number of songs matching the query
          , cPlaytime :: Seconds -- ^ Total play time of matching songs
          }
    deriving (Eq, Show)

parseCount :: [String] -> Either String Count
parseCount = foldM f empty . toAssoc
        where f a ("songs", x)    = parse parseNum
                                    (\x' -> a { cSongs = x'}) x
              f a ("playtime", x) = parse parseNum
                                    (\x' -> a { cPlaytime = x' }) x
              f _ x               = Left $ show x
              empty = Count { cSongs = 0, cPlaytime = 0 }

runParser :: ([String] -> Either String a) -> [String] -> MPD a
runParser f = either (throwError . Unexpected) return . f

-- A helper that runs a parser on a string and, depending, on the
-- outcome, either returns the result of some command applied to the
-- result, or fails. Used when building structures.
parse :: Monad m => (String -> Maybe a) -> (a -> b) -> String -> m b
parse p g x = maybe (fail x) (return . g) (p x)

-- A helper for running a parser returning Maybe on a pair of strings.
-- Returns Just if both strings where parsed successfully, Nothing otherwise.
pair :: (String -> Maybe a) -> (String, String) -> Maybe (a, a)
pair p (x, y) = case (p x, p y) of
                    (Just a, Just b) -> Just (a, b)
                    _                -> Nothing
