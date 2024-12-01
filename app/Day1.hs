{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1 where
import Data.Ord
import Data.List

day1 :: String -> IO ()
day1 input = do
    let l = parse input

    let l1 = sortBy (comparing fst) l
    let l2 = sortBy (comparing snd) l

    let x = zipWith (\(left, _) (_, right) -> abs (left - right)) l1 l2
    print x
    print $ sum x

    return ()


parse :: String -> [(Integer, Integer)]
parse s = parseLine <$> lines s
  where
    parseLine line = (a, b)
      where
        w = read <$> words line
        [a, b] = w

