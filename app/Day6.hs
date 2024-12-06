{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day6 (day6, day6part2) where

import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Parallel.Strategies
import Data.Array (bounds, (!), (//))
import Data.Array.IArray ((!?))
import qualified Data.Array.IArray as A
import Data.Function ((&))
import Data.List (nub, nubBy)
import Linear (V2 (..))
import Text.Megaparsec.Byte (char)
import Text.Show.Pretty (pPrint)
import Util (Map, parseMapChar)
import Prelude hiding (Left, Right, map)

data Direction = Up | Down | Left | Right
    deriving (Show, Eq)

rotate :: Direction -> Direction
rotate = \case
    Up -> Right
    Right -> Down
    Down -> Left
    Left -> Up

data Position = Position
    { coord :: V2 Int
    , dir :: Direction
    }
    deriving (Show, Eq)

runPos :: Position -> Position
runPos pos = Position newCoord pos.dir
  where
    newCoord =
        pos.coord + case pos.dir of
            Up -> V2 (-1) 0
            Down -> V2 1 0
            Left -> V2 0 (-1)
            Right -> V2 0 1

day6 :: String -> IO ()
day6 input = do
    let initialPos = Position (fst $ head $ filter (\e -> snd e == '^') (A.assocs charMap)) Up
    let steps = go initialPos
    pPrint $ length (nubBy (\a b -> a.coord == b.coord) steps)
  where
    charMap = parseMapChar input
    go pos = case sim charMap pos of
        Just next -> next : go next
        Nothing -> []

sim :: Map Char -> Position -> Maybe Position
sim map curPos = do
    let nextPos = runPos curPos
    _ <- map !? nextPos.coord
    let nextNextPos = runPos nextPos
    case map !? nextNextPos.coord of
        Just '#' -> Just $ Position nextPos.coord (rotate nextPos.dir)
        _ -> Just nextPos

day6part2 :: String -> IO ()
day6part2 input = do
    let charMap = parseMapChar input
    let initialPos = Position (fst $ head $ filter (\e -> snd e == '^') (A.assocs charMap)) Up
    let !charMaps = do
            let (V2 x0 y0, V2 xmax ymax) = bounds charMap
            x <- [x0 .. xmax]
            y <- [y0 .. ymax]
            return $ charMap // [(V2 x y, '#')]

    let loops' = (isLoop initialPos <$> charMaps) `using` parList rseq

    pPrint $ length (filter id loops')

isLoop :: Position -> Map Char -> Bool
isLoop curPos map = go curPos []
  where
    go pos steps =
        let
            next = sim map pos
         in
            case next of
                Nothing -> False
                Just n -> (n `elem` steps) || go n (n : steps)
