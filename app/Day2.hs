module Day2 where

import Data.Functor
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

day2 :: String -> IO ()
day2 input = do
    let parsed = parse input

    let x = parsed <&> (\line -> foldLineDescend line || foldLineAscend line)

    print (length $ filter id x)

day2part2 :: String -> IO ()
day2part2 input = do
    let parsed = parse input

    let x = parsed <&> (\line -> foldLineDampenDescend line || foldLineDampenAscend line)

    print (length $ filter id x)

parse :: String -> [[Integer]]
parse input = filter (/= []) $ fmap read . words <$> lines input

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

foldLineDampenWith ::
    (Integer -> Integer -> Bool) -> [Integer] -> (Maybe Bool, Maybe (Integer, Bool), Integer)
foldLineDampenWith f line =
    foldl
        ( \acc next -> traceVal $ case acc of
            (Nothing, d, _) -> (Just True, d, next)
            (Just False, _, _) -> (Just False, Nothing, next)
            (Just True, Nothing, prev) -> case f prev next of
                True -> (Just True, Nothing, next)
                False -> (Just True, Just (prev, False), next)
            (Just True, Just (dampen, False), prev) -> case f dampen next of
                True -> (Just True, Just (dampen, True), next)
                False -> (Just False, Just (dampen, True), next)
            (Just True, Just (dampen, True), prev) -> case f prev next of
                True -> (Just True, Just (dampen, True), next)
                False -> (Just False, Just (dampen, True), next)
        )
        (Nothing, Nothing, head line)
        line

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

foldLineDampenAscend :: [Integer] -> Bool
foldLineDampenAscend = fromJust . fst' . foldLineDampenWith (\prev next -> next - prev <= 3 && next - prev > 0)

foldLineDampenDescend :: [Integer] -> Bool
foldLineDampenDescend = fromJust . fst' . foldLineDampenWith (\prev next -> next - prev >= -3 && next - prev < 0)

traceVal :: (Show a) => a -> a
traceVal x = traceShow x x
