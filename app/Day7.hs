{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7 (day7, day7part2) where

import Control.Monad
import Control.Parallel.Strategies
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Show.Pretty
import Util

type Parser = Parsec Void String

parseLine :: Parser (Int, [Int])
parseLine = do
    x1 <- decimal
    void (string ":")
    xs <- some $ space *> decimal
    return (x1, xs)

run :: Int -> Int -> [Int] -> Bool
run target acc [x] = (acc + x) == target || (acc * x) == target
run target acc (x : xs) = run target (acc + x) xs || run target (acc * x) xs

(|>|) :: Int -> Int -> Int
0 |>| y = y
x |>| y = x * 10 ^ f + y
  where
    f :: Int = ceiling (logBase 10 (fromIntegral y + 1) :: Float)

runB :: Int -> Int -> [Int] -> Bool
runB target acc [x] = (acc + x) == target || (acc * x) == target || (acc |>| x) == target
runB target acc (x : xs) =
    (acc <= target)
        && (runB target (acc + x) xs || runB target (acc * x) xs || runB target (acc |>| x) xs)

dayFor :: _ -> String -> IO ()
dayFor r input = do
    let l = case traverse (runParser parseLine "") (lines input) of
            Right x -> x
            Left b -> error (errorBundlePretty b)
    let results = l <&> \(target, line) -> if r target 0 line then Just target else Nothing
    let results' = results `using` parList rseq
    pPrint results'
    pPrint $ sum (catMaybes results')

day7 :: String -> IO ()
day7 = dayFor run

day7part2 :: String -> IO ()
day7part2 = dayFor runB
