module Util where

import Debug.Trace (traceShow)

import Data.Array.IArray as A

traceVal :: (Show a) => a -> a
traceVal x = traceShow x x

parseMap :: (Char -> a) -> String -> A.Array (Int, Int) a
parseMap mapping = fmap mapping . parseMapChar

parseMapChar :: String -> A.Array (Int, Int) Char
parseMapChar s = A.array ((0, 0), (maxlines, maxchars)) $ do
    (iline, line) <- zip [0 ..] (lines s)
    (ielem, c) <- zip [0 ..] line

    return ((iline, ielem), c)
  where
    maxlines = length (lines s) - 1
    maxchars = length (head $ lines s) - 1
