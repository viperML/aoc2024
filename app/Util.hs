module Util where

import Debug.Trace (traceShow)

import Data.Array.IArray as A
import Linear (V2 (V2))

traceVal :: (Show a) => a -> a
traceVal x = traceShow x x

type Map a = A.Array (V2 Int) a

parseMap :: (Char -> a) -> String -> Map a
parseMap mapping = fmap mapping . parseMapChar

parseMapChar :: String -> Map Char
parseMapChar s = A.array (V2 0 0, V2 maxlines maxchars) $ do
    (iline, line) <- zip [0 ..] (lines s)
    (ielem, c) <- zip [0 ..] line

    return (V2 iline ielem, c)
  where
    maxlines = length (lines s) - 1
    maxchars = length (head $ lines s) - 1
