module Day2 where

import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow)
import Data.Functor

day2 :: String -> IO ()
day2 input = do
    let parsed = parse input

    let x = parsed <&> (\line -> foldLineDescend line || foldLineAscend line)

    print (length $ filter id x)

parse :: String -> [[Integer]]
parse input = filter (/= []) $ parseLine <$> lines input
  where
    parseLine line = read <$> words line

foldLineWith :: (Integer -> Integer -> Bool) -> [Integer] -> (Maybe Bool, Integer)
foldLineWith f line =
    foldl
        ( \acc next -> case acc of
            (Nothing, _) -> (Just True, next)
            (Just False, _) -> (Just False, next)
            (Just True, prev) -> (Just (f prev next), next)
        )
        (Nothing, head line)
        line

foldLineAscend :: [Integer] -> Bool
foldLineAscend = fromJust . fst . foldLineWith (\prev next -> next - prev <= 3 && next - prev > 0)

foldLineDescend :: [Integer] -> Bool
foldLineDescend = fromJust . fst . foldLineWith (\prev next -> next - prev >= -3 && next - prev < 0)
