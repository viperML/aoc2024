module Day6 (day6, day6part2) where

import Text.Show.Pretty (pPrint)
import Util (parseMapChar)

day6 :: String -> IO ()
day6 input = do
    let parsed = parseMapChar input
    pPrint parsed

day6part2 :: String -> IO ()
day6part2 input = do
    undefined
