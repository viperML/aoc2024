{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day1 where

import Data.List
import Data.Ord
import Data.Functor

day1 :: String -> IO ()
day1 input = do
  let l = parse input

  let l1 = sortBy (comparing fst) l
  let l2 = sortBy (comparing snd) l

  let x = zipWith (\(a, _) (_, b) -> abs (a - b)) l1 l2
  print x
  print (sum x)

day1part2 :: String -> IO ()
day1part2 s = do
  let l = parse s

  let x = l <&> (\(a, _) -> a * toInteger (length (filter (\(_, b) -> b == a) l)))
  print x
  print (sum x)

parse :: String -> [(Integer, Integer)]
parse s = parseLine <$> lines s
 where
  parseLine line = (a, b)
   where
    w = read <$> words line
    [a, b] = w
